module FlimFlam.Devices.ATmega328 (withDevice) where

import Numeric.Natural
import Linca.List
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import qualified Linca.ByteString.Lazy as BL
import Data.Typeable
import Control.Monad.Cont
import Control.Exception
import Linca.Cryptography
import Text.Printf
import System.IO
import qualified System.Ftdi as Ftdi

import FlimFlam.Access (PagingLength (PagingLength), PagingAccess (PagingAccess), StorageAccess, MemoryAccess)
import FlimFlam.Segment
import FlimFlam.Paging
import FlimFlam.Memory
import qualified FlimFlam.Device as FlimFlam

data ATmega328Exception =
	UnknownResponseException Command String |
	InvalidResponseException Command Response |
	ResponseErrorException Command |
	ResponseChecksumException Command Word16 Word16
	deriving Typeable

instance Show ATmega328Exception where
	show (UnknownResponseException command message) = printf "%s: received an unknown response (%s)" (show command) message
	show (InvalidResponseException command response) = printf "%s: received an invalid response (%s)" (show command) (show response)
	show (ResponseErrorException command) = printf "%s: received the error response" (show command)
	show (ResponseChecksumException command dataChecksum receivedChecksum) = printf
		"%s: data checksum (0x%04X) did not match received checksum (0x%04X)" (show command) dataChecksum receivedChecksum

instance Exception ATmega328Exception


data Storage = Flash | Eeprom | Sigcal | Fuselock deriving Show

instance Binary Storage where
	put Flash    = putWord16le 0x0000
	put Eeprom   = putWord16le 0x0001
	put Sigcal   = putWord16le 0x0002
	put Fuselock = putWord16le 0x0003
	get = undefined

pagingLength :: Storage -> PagingLength
pagingLength Flash    = PagingLength 0x100 0x80
pagingLength Eeprom   = PagingLength 0x100  0x4
pagingLength Sigcal   = PagingLength  0x10  0x1
pagingLength Fuselock = PagingLength  0x10  0x1


data Command = Exit | Read Storage Word8 | Write Storage Word8 BL.ByteString

instance Show Command where
	show Exit                              = printf "Exit"
	show (Read  storage pageIndex)         = printf "Read %s 0x%02X" (show storage) pageIndex
	show (Write storage pageIndex payload) = printf "Write %s 0x%02X [0x%04X]" (show storage) pageIndex (BL.length payload)

instance Binary Command where
	put Exit                              = putWord16le 0x0000
	put (Read  storage pageIndex)         = putWord16le 0x0001 >> put storage >> putWord8 pageIndex
	put (Write storage pageIndex payload) = putWord16le 0x0002 >> put storage >> putWord8 pageIndex >> putLazyByteString payload
	get = undefined


data Response = Error | SuccessExit | SuccessRead Word16 BL.ByteString | SuccessWrite Word16

instance Show Response where
	show Error                           = printf "Error"
	show SuccessExit                     = printf "SuccessExit"
	show (SuccessRead checksum pageData) = printf "SuccessRead 0x%04X [0x%04X]" checksum (BL.length pageData)
	show (SuccessWrite checksum)         = printf "SuccessWrite 0x%04X" checksum

instance Binary Response where
	get = getWord16le >>= fromId where
		fromId 0x0000 = return Error
		fromId 0x0001 = return SuccessExit
		fromId 0x0002 = do
			checksum <- getWord16le
			length <- getWord16le
			pageData <- getLazyByteString $ fromIntegral length
			return $ SuccessRead checksum pageData
		fromId 0x0003 = do
			checksum <- getWord16le
			return $ SuccessWrite checksum
		fromId responseId = fail $ printf "invalid response id (0x%04X)" responseId
	put = undefined


execute :: Ftdi.Context -> Command -> IO Response
execute context command = do
	hPutStr stderr $ show command
	Ftdi.sendEncode context command
	hPutStr stderr $ " -> "
	response <- Ftdi.receiveDecode context (UnknownResponseException command)
	hPutStr stderr $ show response
	hPutStrLn stderr ""
	return response

exitBootLoader :: Ftdi.Context -> IO ()
exitBootLoader context = execute context command >>= check where
	command = Exit
	check Error = throwIO $ ResponseErrorException command
	check SuccessExit = return ()
	check response = throwIO $ InvalidResponseException command response

readPage :: Ftdi.Context -> Storage -> Natural -> IO BL.ByteString
readPage context storage pageIndex = execute context command >>= check where
	command = Read storage (fromIntegral pageIndex)
	check Error = throwIO $ ResponseErrorException command
	check (SuccessRead checksum pageData)
		| dataChecksum /= checksum = throwIO $ ResponseChecksumException command dataChecksum checksum
		| otherwise = return pageData
		where dataChecksum = BL.fold crc16 pageData 0
	check response = throwIO $ InvalidResponseException command response

writePage :: Ftdi.Context -> Storage -> Natural -> BL.ByteString -> IO ()
writePage context storage pageIndex pageData = execute context command >>= check where
	command = Write storage (fromIntegral pageIndex) pageData
	check Error = throwIO $ ResponseErrorException command
	check (SuccessWrite checksum)
		| dataChecksum /= checksum = throwIO $ ResponseChecksumException command dataChecksum checksum
		| otherwise = return ()
		where dataChecksum = BL.fold crc16 pageData 0
	check response = throwIO $ InvalidResponseException command response


data Memory = Application | BootLoader | Configuration | Signature | Calibration | Fuses | Lock deriving (Enum, Bounded, Show, Read)

segments :: Memory -> [Segment Storage]
segments Application = [rangeSegment Flash 0x0000 0x7000]
segments BootLoader = [rangeSegment Flash 0x7000 0x8000]
segments Configuration = [baseSegment Eeprom 0x400]
segments Signature = byteSegment Sigcal <$> [0x0, 0x2, 0x4]
segments Calibration = byteSegment Sigcal <$> [0x1]
segments Fuses = byteSegment Fuselock <$> [0x0, 0x3, 0x2]
segments Lock = byteSegment Fuselock <$> [0x1]


pagingAccess :: Ftdi.Context -> Storage -> PagingAccess IO
pagingAccess context storage = PagingAccess (pagingLength storage) (readPage context storage) (writePage context storage)

storageAccess :: Ftdi.Context -> Storage -> StorageAccess IO
storageAccess context storage = pagedStorageAccess (pagingAccess context storage)

memoryAccess :: Ftdi.Context -> Memory -> MemoryAccess IO
memoryAccess context memory = storedMemoryAccess (storageAccess context) (segments memory)


withDevice :: ContT result IO (FlimFlam.Device Memory)
withDevice = do
	let device = Ftdi.Device { Ftdi.vendorID = 0x0403, Ftdi.productID = 0x6001, Ftdi.index = 0}
	let parameters = Ftdi.Parameters { Ftdi.baudRate = 20000 }
	context <- Ftdi.withContext device parameters
	return $ FlimFlam.Device enum show read (exitBootLoader context) (memoryAccess context)
