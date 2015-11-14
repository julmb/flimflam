module FlimFlam.FirmwareCommand (FirmwareCommand (..)) where

import Data.Word
import Data.Binary
import Data.Binary.Put
import FlimFlam.Memory

data FirmwareCommand = Exit | ReadDeviceInformation | ReadMemoryPage MemoryType Word8 | WriteMemoryPage MemoryType Word8 deriving (Eq, Show, Read)

instance Binary FirmwareCommand where
	put Exit                                   = putWord16le 0x0000
	put ReadDeviceInformation                  = putWord16le 0x0001
	put (ReadMemoryPage  memoryType pageIndex) = putWord16le 0x0002 >> put memoryType >> put pageIndex
	put (WriteMemoryPage memoryType pageIndex) = putWord16le 0x0003 >> put memoryType >> put pageIndex
	get = undefined
