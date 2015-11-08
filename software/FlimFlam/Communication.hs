module FlimFlam.Communication where

import Data.Monoid
import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import qualified Linca.ByteString.Lazy as BL
import Control.Monad
import Control.Exception
import Text.Printf
import System.IO
import System.Ftdi
import Linca.Basic
import Linca.Scalar
import FlimFlam.Memory
import FlimFlam.FirmwareCommand
import FlimFlam.DeviceInformation
import FlimFlam.Exception

executeFirmwareCommand :: Context -> FirmwareCommand -> Integer -> BL.ByteString -> IO BL.ByteString
executeFirmwareCommand context command responseLength commandData = do
	hPutStrLn stderr $ printf "executing firmware command %s, expected response length %d..." (show command) responseLength
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


getDeviceInformation :: Context -> IO DeviceInformation
getDeviceInformation context = executeFirmwareCommand context GetDeviceInformation deviceInformationLength mempty >>= return . decode

readPage :: Context -> DeviceInformation -> MemoryType -> Word8 -> IO BL.ByteString
readPage context deviceInformation memoryType index = do
	let pageLength = memoryPageLength $ memoryInformation deviceInformation memoryType
	executeFirmwareCommand context (ReadPage memoryType index) (fromIntegral pageLength) mempty

writePage :: Context -> DeviceInformation -> MemoryType -> Word8 -> BL.ByteString -> IO ()
writePage context deviceInformation memoryType index pageData = do
	let pageLength = memoryPageLength $ memoryInformation deviceInformation memoryType
	when (BL.length pageData /= fromIntegral pageLength) $
		error $ printf "writePage: length of parameter pageData (0x%04X) did not match page length (0x%04X)" (BL.length pageData) pageLength
	executeFirmwareCommand context (WritePage memoryType index) 0 pageData >> return ()


readFromPage :: Context -> DeviceInformation -> MemoryType -> Word8 -> Word16 -> Word16 -> IO BL.ByteString
readFromPage context deviceInformation memoryType index offset length = do
	pageData <- readPage context deviceInformation memoryType index
	return $ BL.take (fromIntegral length) $ BL.drop (fromIntegral offset) $ pageData

writeToPage :: Context -> DeviceInformation -> MemoryType -> Word8 -> Word16 -> BL.ByteString -> IO ()
writeToPage context deviceInformation memoryType index offset chunk
	| offset == 0 && chunkLength == fromIntegral pageLength = writePage context deviceInformation memoryType index chunk
	| otherwise = do
		pageData <- readPage context deviceInformation memoryType index
		let newPageData = BL.take (fromIntegral offset) pageData <> chunk <> BL.drop (fromIntegral offset + chunkLength) pageData
		writePage context deviceInformation memoryType index newPageData
	where
		chunkLength = BL.length chunk
		pageLength = memoryPageLength $ memoryInformation deviceInformation memoryType


readMemory :: Context -> DeviceInformation -> MemoryType -> Word16 -> Word16 -> IO BL.ByteString
readMemory _ _ _ _ 0 = return mempty
readMemory context deviceInformation memoryType offset length = do
	let pageLength = memoryPageLength $ memoryInformation deviceInformation memoryType
	let (pageIndex, pageOffset) = normalize pageLength (0, offset)
	let readLength = min length (pageLength - pageOffset)
	readChunk <- readFromPage context deviceInformation memoryType pageIndex pageOffset readLength
	readRest <- readMemory context deviceInformation memoryType (offset + readLength) (length - readLength)
	return (readChunk <> readRest)

writeMemory :: Context -> DeviceInformation -> MemoryType -> Word16 -> Word16 -> BL.ByteString -> IO ()
writeMemory _ _ _ _ 0 _ = return ()
writeMemory context deviceInformation memoryType offset length writeData = do
	let pageLength = memoryPageLength $ memoryInformation deviceInformation memoryType
	let (pageIndex, pageOffset) = normalize pageLength (0, offset)
	let writeLength = min length (pageLength - pageOffset)
	writeToPage context deviceInformation memoryType pageIndex pageOffset (BL.take (fromIntegral writeLength) writeData)
	writeMemory context deviceInformation memoryType (offset + writeLength) (length - writeLength) (BL.drop (fromIntegral writeLength) writeData)
