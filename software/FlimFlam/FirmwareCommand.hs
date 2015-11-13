module FlimFlam.FirmwareCommand (FirmwareCommand (..)) where

import Data.Word
import Data.Binary
import Data.Binary.Put
import FlimFlam.Memory

data FirmwareCommand = GetDeviceInformation | ReadPage MemoryType Word8 | WritePage MemoryType Word8 deriving (Eq, Show, Read)

instance Binary FirmwareCommand where
	put GetDeviceInformation = putWord16le 0x0001
	put (ReadPage memoryType pageIndex) = putWord16le 0x0002 >> put memoryType >> put pageIndex
	put (WritePage memoryType pageIndex) = putWord16le 0x0003 >> put memoryType >> put pageIndex
	get = undefined
