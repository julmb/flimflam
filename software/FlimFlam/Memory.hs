module FlimFlam.Memory where

import Data.Binary
import Data.Binary.Put
import Linca.Basic

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
