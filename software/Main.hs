import Data.Monoid
import Data.Word
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import qualified Linca.ByteString.Lazy as BL
import Data.Typeable
import Control.Monad
import Control.Exception
import Text.Printf
import System.IO
import System.Environment
import System.Ftdi
import Linca.Basic

data FlimFlamException = ResponseException String String deriving Typeable

instance Show FlimFlamException where
	show (ResponseException location message) = location ++ ": " ++ message
instance Exception FlimFlamException

-- TODO: see what happens when we write to bootloader flash and read it back before and afterwards
-- TODO: see what happens when we read beyond the address space
-- TODO: when writing a program, we should probably pad with 0x0000 (align correctly, maybe reject images with odd size), 0x0000 is nop and will skip to bootloader
-- TODO: should we be able to parse hex? or just add the objcopy stuff to the makefile for easy binary generation?
-- TODO: add flimflam execution to application makefile to automatically flash the MCU
-- TODO: can we use the parser monad Get for something?
-- TODO: can we get rid of most of the bytestring dependencies, or at least the qualified import
-- TODO: implement CRC16 checking + ack
-- TODO: extract modules

data MemoryType = Flash | Eeprom | Calibration | Fuse | Lock | Signature deriving (Eq, Show, Read)

instance Binary MemoryType where
	put Flash = putWord16le 0x0001
	put Eeprom = putWord16le 0x0002
	put Calibration = putWord16le 0x0003
	put Fuse = putWord16le 0x0004
	put Lock = putWord16le 0x0005
	put Signature = putWord16le 0x0006
	get = undefined

data FirmwareCommand = GetPageCount MemoryType | GetPageLength MemoryType | ReadPage MemoryType Word8 | WritePage MemoryType Word8 deriving (Eq, Show, Read)

instance Binary FirmwareCommand where
	put (GetPageCount memoryType) = putWord16le 0x0001 >> put memoryType
	put (GetPageLength memoryType) = putWord16le 0x0002 >> put memoryType
	put (ReadPage memoryType pageIndex) = putWord16le 0x0003 >> put memoryType >> put pageIndex
	put (WritePage memoryType pageIndex) = putWord16le 0x0004 >> put memoryType >> put pageIndex
	get = undefined

getResponseLength :: Context -> FirmwareCommand -> IO Integer
getResponseLength _ (GetPageCount _) = return 2
getResponseLength _ (GetPageLength _) = return 2
getResponseLength context (ReadPage memoryType _) = getPageLength context memoryType >>= return . fromIntegral
getResponseLength _ (WritePage _ _) = return 0

executeFirmwareCommand :: Context -> FirmwareCommand -> BL.ByteString -> IO BL.ByteString
executeFirmwareCommand context firmwareCommand commandData = do
	hPutStrLn stderr (show firmwareCommand)
	responseLength <- getResponseLength context firmwareCommand
	send context (encode firmwareCommand)
	send context commandData
	response <- receive context responseLength
	appendix <- receive context 2
	let dataChecksum = BL.fold crc16 response 0
	let receivedChecksum = runGet getWord16le appendix
	when (dataChecksum /= receivedChecksum) $ do
		let message = printf "data checksum (0x%04X) did not match received checksum (0x%04X)" dataChecksum receivedChecksum
		throwIO $ ResponseException "executeFirmwareCommand" message
	return response

getPageCount :: Context -> MemoryType -> IO Word16
getPageCount context memoryType = executeFirmwareCommand context (GetPageCount memoryType) mempty >>= return . runGet getWord16le

getPageLength :: Context -> MemoryType -> IO Word16
getPageLength context memoryType = executeFirmwareCommand context (GetPageLength memoryType) mempty >>= return . runGet getWord16le

readPage :: Context -> MemoryType -> Word8 -> IO BL.ByteString
readPage context memoryType pageIndex = executeFirmwareCommand context (ReadPage memoryType pageIndex) mempty

writePage :: Context -> MemoryType -> Word8 -> BL.ByteString -> IO ()
writePage context memoryType pageIndex pageData = executeFirmwareCommand context (ReadPage memoryType pageIndex) pageData >> return ()

data Command = Program | Configure | Dump MemoryType Word16 Word16 | Load MemoryType Word16 deriving (Eq, Show, Read)

readMemory :: Context -> MemoryType -> Word16 -> Word16 -> IO BL.ByteString
readMemory _ _ _ 0 = return mempty
readMemory context memoryType position length = do
	pageLength <- getPageLength context memoryType
	let pageIndex = fromIntegral (position `div` pageLength)
	let size = min pageLength length
	page <- readPage context memoryType pageIndex
	rest <- readMemory context memoryType (position + size) (length - size)
	return (BL.take (fromIntegral size) page <> rest)

-- TODO: can we start outputting stuff before we're finished reading?
executeCommand :: Context -> Command -> IO ()
executeCommand context (Dump memoryType position length) = readMemory context memoryType position length >>= BL.putStr

main :: IO ()
main = do
	let device = Device { vendorID = 0x0403, productID = 0x6001, System.Ftdi.index = 0}
	let parameters = Parameters { baudRate = 20000 }
	command <- getArgs >>= return . read . unwords
	let run context = executeCommand context command
	withContext device parameters run
