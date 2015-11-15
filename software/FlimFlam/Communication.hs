module FlimFlam.Communication
(
	executeFirmwareCommand,
	runApplication, readDeviceInformation, readMemoryPage, writeMemoryPage,
	readFromMemoryPage, writeToMemoryPage, readMemory, writeMemory, writeVerifyMemory
)
where

import Numeric.Natural
import Data.Monoid
import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import Control.Monad
import Control.Exception
import Text.Printf
import System.IO
import System.Ftdi
import Linca.Scalar
import qualified Linca.ByteString as BL
import Linca.Cryptography
import FlimFlam.Memory
import FlimFlam.FirmwareCommand
import FlimFlam.DeviceInformation
import FlimFlam.Exception

executeFirmwareCommand :: Context -> FirmwareCommand -> Natural -> BL.ByteString -> IO BL.ByteString
executeFirmwareCommand context command responseLength commandData = do
	hPutStrLn stderr $ show command
	send context (encode command)
	send context commandData
	response <- receive context responseLength
	appendix <- receive context 2
	let dataChecksum = BL.fold crc16 response 0
	let receivedChecksum = runGet getWord16le appendix
	when (dataChecksum /= receivedChecksum) $ do
		let message = printf "data checksum (0x%04X) did not match received checksum (0x%04X)" dataChecksum receivedChecksum
		throwIO $ ResponseException command message
	return response

runApplication :: Context -> IO ()
runApplication context = executeFirmwareCommand context Exit 0 mempty >> return ()

readDeviceInformation :: Context -> IO DeviceInformation
readDeviceInformation context = executeFirmwareCommand context ReadDeviceInformation deviceInformationLength mempty >>= return . decode

readMemoryPage :: Context -> DeviceInformation -> MemoryType -> Natural -> IO BL.ByteString
readMemoryPage context deviceInformation memoryType pageIndex
	| pageIndex >= pageCount = throwIO $ InputException "readMemoryPage" $ printf "pageIndex (0x%X) was greater than or equal to the page count (0x%X)" pageIndex pageCount
	| otherwise = executeFirmwareCommand context (ReadMemoryPage memoryType (fromIntegral pageIndex)) pageLength mempty
	where
		pageCount = memoryPageCount deviceInformation memoryType
		pageLength = memoryPageLength deviceInformation memoryType

writeMemoryPage :: Context -> DeviceInformation -> MemoryType -> Natural -> BL.ByteString -> IO ()
writeMemoryPage context deviceInformation memoryType pageIndex pageData
	| pageIndex >= pageCount = throwIO $ InputException "writeMemoryPage" $ printf "pageIndex (0x%X) was greater than or equal to the page count (0x%X)" pageIndex pageCount
	| pageDataLength /= pageLength = throwIO $ InputException "writeMemoryPage" $ printf "length of pageData (0x%X) did not match the page length (0x%X)" pageDataLength pageLength
	| otherwise = executeFirmwareCommand context (WriteMemoryPage memoryType (fromIntegral pageIndex)) 0 pageData >> return ()
	where
		pageCount = memoryPageCount deviceInformation memoryType
		pageLength = memoryPageLength deviceInformation memoryType
		pageDataLength = fromIntegral (BL.length pageData)


readFromMemoryPage :: Context -> DeviceInformation -> MemoryType -> Natural -> Natural -> Natural -> IO BL.ByteString
readFromMemoryPage context deviceInformation memoryType pageIndex pageOffset length
	| pageIndex >= pageCount = throwIO $ InputException "readFromMemoryPage" $ printf "pageIndex (0x%X) was greater than or equal to the page count (0x%X)" pageIndex pageCount
	| pageOffset + length > pageLength = throwIO $ InputException "readFromMemoryPage" $ printf "pageOffset + length (0x%X) was greater than the page length (0x%X)" (pageOffset + length) pageLength
	| length == 0 = return mempty
	| otherwise = do
		pageData <- readMemoryPage context deviceInformation memoryType pageIndex
		return $ BL.take (fromIntegral length) $ BL.drop (fromIntegral pageOffset) $ pageData
	where
		pageCount = memoryPageCount deviceInformation memoryType
		pageLength = memoryPageLength deviceInformation memoryType

writeToMemoryPage :: Context -> DeviceInformation -> MemoryType -> Natural -> Natural -> BL.ByteString -> IO ()
writeToMemoryPage context deviceInformation memoryType pageIndex pageOffset chunk
	| pageIndex >= pageCount = throwIO $ InputException "writeToMemoryPage" $ printf "pageIndex (0x%X) was greater than or equal to the page count (0x%X)" pageIndex pageCount
	| pageOffset + length > pageLength = throwIO $ InputException "writeToMemoryPage" $ printf "pageOffset + length (0x%X) was greater than the page length (0x%X)" (pageOffset + length) pageLength
	| length == 0 = return ()
	| pageOffset == 0 && length == pageLength = writeMemoryPage context deviceInformation memoryType pageIndex chunk
	| otherwise = do
		pageData <- readMemoryPage context deviceInformation memoryType pageIndex
		let newPageData = BL.take (fromIntegral pageOffset) pageData <> chunk <> BL.drop (fromIntegral (pageOffset + length)) pageData
		writeMemoryPage context deviceInformation memoryType pageIndex newPageData
	where
		pageCount = memoryPageCount deviceInformation memoryType
		pageLength = memoryPageLength deviceInformation memoryType
		length = fromIntegral (BL.length chunk)


readMemory :: Context -> DeviceInformation -> MemoryType -> Natural -> Natural -> IO BL.ByteString
readMemory context deviceInformation memoryType offset length
	| offset + length > memoryLength = throwIO $ InputException "readMemory" $ printf "offset + length (0x%X) was greater than the memory length (0x%X)" (offset + length) memoryLength
	| length == 0 = return mempty
	| otherwise = do
		let (pageIndex, pageOffset) = normalize pageLength (0, offset)
		let readLength = min length (pageLength - pageOffset)
		readChunk <- readFromMemoryPage context deviceInformation memoryType pageIndex pageOffset readLength
		readRest <- readMemory context deviceInformation memoryType (offset + readLength) (length - readLength)
		return (readChunk <> readRest)
	where
		pageCount = memoryPageCount deviceInformation memoryType
		pageLength = memoryPageLength deviceInformation memoryType
		memoryLength = pageCount * pageLength

writeMemory :: Context -> DeviceInformation -> MemoryType -> Natural -> BL.ByteString -> IO ()
writeMemory context deviceInformation memoryType offset chunk
	| offset + length > memoryLength = throwIO $ InputException "writeMemory" $ printf "offset + length (0x%X) was greater than the memory length (0x%X)" (offset + length) memoryLength
	| length == 0 = return ()
	| otherwise = do
		let (pageIndex, pageOffset) = normalize pageLength (0, offset)
		let writeLength = min length (pageLength - pageOffset)
		writeToMemoryPage context deviceInformation memoryType pageIndex pageOffset (BL.take (fromIntegral writeLength) chunk)
		writeMemory context deviceInformation memoryType (offset + writeLength) (BL.drop (fromIntegral writeLength) chunk)
	where
		pageCount = memoryPageCount deviceInformation memoryType
		pageLength = memoryPageLength deviceInformation memoryType
		memoryLength = pageCount * pageLength
		length = fromIntegral (BL.length chunk)

writeVerifyMemory :: Context -> DeviceInformation -> MemoryType -> Natural -> BL.ByteString -> IO ()
writeVerifyMemory context deviceInformation memoryType offset writeData
	| offset + length > memoryLength = throwIO $ InputException "writeVerifyMemory" $ printf "offset + length (0x%X) was greater than the memory length (0x%X)" (offset + length) memoryLength
	| otherwise = do
		writeMemory context deviceInformation memoryType offset writeData
		readData <- readMemory context deviceInformation memoryType offset length
		when (writeData /= readData) $ throwIO VerificationException
	where
		pageCount = memoryPageCount deviceInformation memoryType
		pageLength = memoryPageLength deviceInformation memoryType
		memoryLength = pageCount * pageLength
		length = fromIntegral (BL.length writeData)
