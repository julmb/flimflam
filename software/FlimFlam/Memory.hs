module FlimFlam.Memory (MemoryType (..), memoryTypes) where

import Data.Binary
import Data.Binary.Put
import Linca.List

data MemoryType = Flash | Eeprom deriving (Eq, Show, Read, Enum, Bounded)

instance Binary MemoryType where
	put Flash = putWord16le 0x0001
	put Eeprom = putWord16le 0x0002
	get = undefined

memoryTypes :: [MemoryType]
memoryTypes = enum
