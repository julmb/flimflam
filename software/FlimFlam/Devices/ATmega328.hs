module FlimFlam.Devices.ATmega328 (device) where

import Numeric.Natural
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Linca.List
import Linca.Size
import FlimFlam.Access (PagingLength (..), pagingTotalLength, PagingAccess (PagingAccess), StorageAccess, MemoryAccess)
import FlimFlam.Segment (Segment, rangeSegment, baseSegment, byteSegments)
import FlimFlam.Paging
import FlimFlam.Memory
import FlimFlam.Device (Device (Device))

import qualified Data.ByteString.Lazy as BL
import System.IO
import System.Ftdi (Context, send, receive)
import qualified Linca.ByteString as BL
import Linca.Cryptography

data Storage = Flash | Eeprom | Sigcal | Fuselock deriving (Eq, Ord, Enum, Bounded, Show, Read)

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

instance Size Storage where
	size = pagingTotalLength . pagingLength


data Page = Page Storage Word16 deriving (Eq, Show, Read)

instance Binary Page where
	put (Page storage pageIndex) = put storage >> putWord16le pageIndex
	get = undefined

instance Size Page where
	size (Page storage _) = pageLength $ pagingLength storage


data Command = Exit | Read Page | Write Page deriving (Eq, Show, Read)

instance Binary Command where
	put Exit         = putWord16le 0x0000
	put (Read page)  = putWord16le 0x0001 >> put page
	put (Write page) = putWord16le 0x0002 >> put page
	get = undefined

responseLength :: Command -> Natural
responseLength Exit = 0
responseLength (Read page) = size page
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
--	when (dataChecksum /= receivedChecksum) $ do
--		let message = printf "data checksum (0x%04X) did not match received checksum (0x%04X)" dataChecksum receivedChecksum
--		throwIO $ ResponseException command message
	return response


data Memory = Application | BootLoader | Configuration | Signature | Calibration | Fuses | Lock deriving (Eq, Ord, Enum, Bounded, Show, Read)

segments :: Memory -> [Segment Storage]
segments Application = [rangeSegment Flash 0x0000 0x7000]
segments BootLoader = [rangeSegment Flash 0x7000 0x8000]
segments Configuration = [baseSegment Eeprom 0x400]
segments Signature = byteSegments Sigcal [0x00, 0x02, 0x04]
segments Calibration = byteSegments Sigcal [0x01]
segments Fuses = byteSegments Fuselock [0x00, 0x03, 0x02]
segments Lock = byteSegments Fuselock [0x01]

instance Size Memory where
	size = size . segments


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
