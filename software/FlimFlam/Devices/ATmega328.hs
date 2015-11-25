module FlimFlam.Devices.ATmega328 (device) where

import Numeric.Natural
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Typeable
import Control.Monad
import Control.Exception
import Text.Printf
import Linca.List
import FlimFlam.Access (PagingLength (..), PagingAccess (PagingAccess), StorageAccess, MemoryAccess)
import FlimFlam.Segment (Segment, rangeSegment, baseSegment, byteSegment)
import FlimFlam.Paging
import FlimFlam.Memory
import FlimFlam.Device (Device (Device))

import qualified Data.ByteString.Lazy as BL
import System.IO
import System.Ftdi (Context, send, receive)
import qualified Linca.ByteString as BL
import Linca.Cryptography

data ATmega328Exception = ChecksumException Command Word16 Word16 deriving Typeable

instance Show ATmega328Exception where
	show (ChecksumException command dataChecksum receivedChecksum) = printf
		"%s: data checksum (0x%04X) did not match received checksum (0x%04X)" (show command) dataChecksum receivedChecksum

instance Exception ATmega328Exception


data Storage = Flash | Eeprom | Sigcal | Fuselock deriving (Eq, Show, Read)

instance Binary Storage where
	put Flash    = putWord16le 0x0000
	put Eeprom   = putWord16le 0x0001
	put Sigcal   = putWord16le 0x0002
	put Fuselock = putWord16le 0x0003
	get = undefined

pagingLength :: Storage -> PagingLength
pagingLength Flash    = PagingLength 0x100 0x80
pagingLength Eeprom   = PagingLength 0x100  0x4
pagingLength Sigcal   = PagingLength   0x1 0x10
pagingLength Fuselock = PagingLength   0x1 0x10


data Page = Page Storage Word16 deriving (Eq, Show, Read)

instance Binary Page where
	put (Page storage pageIndex) = put storage >> putWord16le pageIndex
	get = undefined


data Command = Exit | Read Page | Write Page deriving (Eq, Show, Read)

instance Binary Command where
	put Exit         = putWord16le 0x0000
	put (Read page)  = putWord16le 0x0001 >> put page
	put (Write page) = putWord16le 0x0002 >> put page
	get = undefined

responseLength :: Command -> Natural
responseLength Exit = 0
responseLength (Read (Page storage _)) = pageLength $ pagingLength storage
responseLength (Write _) = 0

executeCommand :: Context -> Command -> BL.ByteString -> IO BL.ByteString
executeCommand context command commandData = do
	hPutStrLn stderr $ show command
	send context (encode command)
	send context commandData
	response <- receive context (responseLength command)
	appendix <- receive context 2
	let dataChecksum = BL.fold crc16 response 0
	let receivedChecksum = runGet getWord16le appendix
	when (dataChecksum /= receivedChecksum) $ throwIO (ChecksumException command dataChecksum receivedChecksum)
	return response


data Memory = Application | BootLoader | Configuration | Signature | Calibration | Fuses | Lock deriving (Eq, Ord, Enum, Bounded, Show, Read)

segments :: Memory -> [Segment Storage]
segments Application = [rangeSegment Flash 0x0000 0x7000]
segments BootLoader = [rangeSegment Flash 0x7000 0x8000]
segments Configuration = [baseSegment Eeprom 0x400]
segments Signature = map (byteSegment Sigcal) [0x0, 0x2, 0x4]
segments Calibration = map (byteSegment Sigcal) [0x1]
segments Fuses = map (byteSegment Fuselock) [0x0, 0x3, 0x2]
segments Lock = map (byteSegment Fuselock) [0x1]


pagingAccess :: Context -> Storage -> PagingAccess IO
pagingAccess context storage = PagingAccess (pagingLength storage) readPage writePage where
	readPage pageIndex = executeCommand context (Read $ Page storage (fromIntegral pageIndex)) mempty
	writePage pageIndex pageData = executeCommand context (Write $ Page storage (fromIntegral pageIndex)) pageData >> return ()

storageAccess :: Context -> Storage -> StorageAccess IO
storageAccess context storage = pagedStorageAccess (pagingAccess context storage)

memoryAccess :: Context -> Memory -> MemoryAccess IO
memoryAccess context memory = storedMemoryAccess (storageAccess context) (segments memory)


device :: Context -> Device Memory
device context = Device enum show read runApplication (memoryAccess context) where
	runApplication = executeCommand context Exit mempty >> return ()
