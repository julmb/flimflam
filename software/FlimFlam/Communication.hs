module FlimFlam.Communication (executeFirmwareCommand, runApplication, readDeviceInformation, readMemoryPage, writeMemoryPage) where

import Numeric.Natural
import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import Control.Monad
import Control.Exception
import Text.Printf
import System.IO
import System.Ftdi
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
