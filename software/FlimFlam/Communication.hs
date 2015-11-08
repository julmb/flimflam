module FlimFlam.Communication where

import Numeric.Natural
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

executeFirmwareCommand :: Context -> FirmwareCommand -> Natural -> BL.ByteString -> IO BL.ByteString
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

-- TODO: shorten where part
readPage :: Context -> DeviceInformation -> MemoryType -> Natural -> IO BL.ByteString
readPage context deviceInformation memoryType pageIndex
	| pageIndex >= pageCount = error $ printf "readPage: parameter pageIndex (0x%X) was greater than or equal to the page count (0x%X)" pageIndex pageCount
	| otherwise = executeFirmwareCommand context (ReadPage memoryType (fromIntegral pageIndex)) pageLength mempty
	where
		pageCount = memoryPageCount $ memoryInformation deviceInformation memoryType
		pageLength = memoryPageLength $ memoryInformation deviceInformation memoryType

writePage :: Context -> DeviceInformation -> MemoryType -> Natural -> BL.ByteString -> IO ()
writePage context deviceInformation memoryType pageIndex pageData
	| pageIndex >= pageCount = error $ printf "writePage: parameter pageIndex (0x%X) was greater than or equal to the page count (0x%X)" pageIndex pageCount
	| pageLength /= pageDataLength = error $ printf "writePage: length of parameter pageData (0x%X) did not match page length (0x%X)" pageDataLength pageLength
	| otherwise = executeFirmwareCommand context (WritePage memoryType (fromIntegral pageIndex)) 0 pageData >> return ()
	where
		pageCount = memoryPageCount $ memoryInformation deviceInformation memoryType
		pageLength = memoryPageLength $ memoryInformation deviceInformation memoryType
		pageDataLength = fromIntegral (BL.length pageData)


-- TODO: error handling, or is the low-level one enough? what is missing here?
readFromPage :: Context -> DeviceInformation -> MemoryType -> Natural -> Natural -> Natural -> IO BL.ByteString
readFromPage context deviceInformation memoryType index offset length = do
	pageData <- readPage context deviceInformation memoryType index
	return $ BL.take (fromIntegral length) $ BL.drop (fromIntegral offset) $ pageData

writeToPage :: Context -> DeviceInformation -> MemoryType -> Natural -> Natural -> BL.ByteString -> IO ()
writeToPage context deviceInformation memoryType index offset chunk
	| offset == 0 && chunkLength == pageLength = writePage context deviceInformation memoryType index chunk
	| otherwise = do
		pageData <- readPage context deviceInformation memoryType index
		let newPageData = BL.take (fromIntegral offset) pageData <> chunk <> BL.drop (fromIntegral (offset + chunkLength)) pageData
		writePage context deviceInformation memoryType index newPageData
	where
		pageLength = memoryPageLength $ memoryInformation deviceInformation memoryType
		chunkLength = fromIntegral (BL.length chunk)


readMemory :: Context -> DeviceInformation -> MemoryType -> Natural -> Natural -> IO BL.ByteString
readMemory _ _ _ _ 0 = return mempty
readMemory context deviceInformation memoryType offset length = do
	let (pageIndex, pageOffset) = normalize pageLength (0, offset)
	let readLength = min length (pageLength - pageOffset)
	readChunk <- readFromPage context deviceInformation memoryType pageIndex pageOffset readLength
	readRest <- readMemory context deviceInformation memoryType (offset + readLength) (length - readLength)
	return (readChunk <> readRest)
	where
		pageLength = memoryPageLength $ memoryInformation deviceInformation memoryType

writeMemory :: Context -> DeviceInformation -> MemoryType -> Natural -> Natural -> BL.ByteString -> IO ()
writeMemory _ _ _ _ 0 _ = return ()
writeMemory context deviceInformation memoryType offset length chunk = do
	when (length > chunkLength) $
		error $ printf "writeMemory: parameter length (0x%04X) was larger than the length of parameter writeData (0x%04X)" length chunkLength
	let (pageIndex, pageOffset) = normalize pageLength (0, offset)
	let writeLength = min length (pageLength - pageOffset)
	writeToPage context deviceInformation memoryType pageIndex pageOffset (BL.take (fromIntegral writeLength) chunk)
	writeMemory context deviceInformation memoryType (offset + writeLength) (length - writeLength) (BL.drop (fromIntegral writeLength) chunk)
	where
		pageLength = memoryPageLength $ memoryInformation deviceInformation memoryType
		chunkLength = fromIntegral (BL.length chunk)
