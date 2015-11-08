import Data.Monoid
import Data.Word
import Data.Maybe
import Data.List
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
import Linca.Scalar

data FlimFlamException = ResponseException FirmwareCommand String deriving Typeable

instance Show FlimFlamException where
	show (ResponseException command message) = printf "response exception %s: %s" (show command) message
instance Exception FlimFlamException

-- TODO: see what happens when we write to bootloader flash and read it back before and afterwards
-- TODO: see what happens when we read beyond the address space
-- TODO: when writing a program, we should probably pad with 0x0000 (align correctly, maybe reject images with odd size), 0x0000 is nop and will skip to bootloader
-- TODO: should we be able to parse hex? or just add the objcopy stuff to the makefile for easy binary generation?
-- TODO: add flimflam execution to application makefile to automatically flash the MCU
-- TODO: can we use the parser monad Get for something?
-- TODO: can we get rid of most of the bytestring dependencies, or at least the qualified import
-- TODO: extract modules

data MemoryType = Flash | Eeprom | Calibration | Fuse | Lock | Signature deriving (Eq, Show, Read, Enum, Bounded)

instance Binary MemoryType where
	put Flash = putWord16le 0x0001
	put Eeprom = putWord16le 0x0002
	put Calibration = putWord16le 0x0003
	put Fuse = putWord16le 0x0004
	put Lock = putWord16le 0x0005
	put Signature = putWord16le 0x0006
	get = undefined

memoryTypes :: [MemoryType]
memoryTypes = enum


data FirmwareCommand = GetDeviceInformation | ReadPage MemoryType Word8 | WritePage MemoryType Word8 deriving (Eq, Show, Read)

instance Binary FirmwareCommand where
	put GetDeviceInformation = putWord16le 0x0001
	put (ReadPage memoryType pageIndex) = putWord16le 0x0002 >> put memoryType >> put pageIndex
	put (WritePage memoryType pageIndex) = putWord16le 0x0003 >> put memoryType >> put pageIndex
	get = undefined

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


data MemoryInformation = MemoryInformation { memoryPageCount :: Word16, memoryPageLength :: Word16 } deriving (Eq, Show, Read)

instance Binary MemoryInformation where
	put = undefined
	get = MemoryInformation <$> getWord16le <*> getWord16le

memoryInformationLength :: Integer
memoryInformationLength = 2 + 2

-- TODO: newtype? record?
data DeviceInformation = DeviceInformation [MemoryInformation] deriving (Eq, Show, Read)

instance Binary DeviceInformation where
	put = undefined
	get = traverse (const get) memoryTypes >>= return . DeviceInformation

deviceInformationLength :: Integer
deviceInformationLength = genericLength memoryTypes * memoryInformationLength

memoryInformation :: DeviceInformation -> MemoryType -> MemoryInformation
memoryInformation (DeviceInformation memoryInformation) memoryType = fromJust (lookup memoryType (zip memoryTypes memoryInformation))


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


data Command = Program | Configure | Dump MemoryType Word16 Word16 | Load MemoryType Word16 | Command FirmwareCommand Integer deriving (Eq, Show, Read)

executeCommand :: Context -> Command -> IO ()
executeCommand context (Dump memoryType position length) = do
	deviceInformation <- getDeviceInformation context
	readData <- readMemory context deviceInformation memoryType position length
	BL.putStr readData
executeCommand context (Load memoryType position) = do
	deviceInformation <- getDeviceInformation context
	writeData <- BL.getContents
	writeMemory context deviceInformation memoryType position (fromIntegral (BL.length writeData)) writeData
executeCommand context (Command command responseLength) = BL.getContents >>= executeFirmwareCommand context command responseLength >>= BL.putStr

-- TODO: add help screen
main :: IO ()
main = do
	let device = Device { vendorID = 0x0403, productID = 0x6001, System.Ftdi.index = 0}
	let parameters = Parameters { baudRate = 20000 }
	hPutStrLn stderr $ printf "parsing parameters..."
	command <- getArgs >>= return . read . unwords
	let run context = do
		hPutStrLn stderr $ printf "executing command..."
		executeCommand context command
	hPutStrLn stderr $ printf "opening FTDI device..."
	withContext device parameters run
