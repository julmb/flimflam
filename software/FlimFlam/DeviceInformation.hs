module FlimFlam.DeviceInformation where

import Data.Maybe
import Data.List
import Data.Binary
import Data.Binary.Get
import FlimFlam.Memory

data MemoryInformation = MemoryInformation { memoryPageCount :: Word16, memoryPageLength :: Word16 } deriving (Eq, Show, Read)

instance Binary MemoryInformation where
	put = undefined
	get = MemoryInformation <$> getWord16le <*> getWord16le

memoryInformationLength :: Integer
memoryInformationLength = 2 + 2

newtype DeviceInformation = DeviceInformation [MemoryInformation] deriving (Eq, Show, Read)

instance Binary DeviceInformation where
	put = undefined
	get = traverse (const get) memoryTypes >>= return . DeviceInformation

deviceInformationLength :: Integer
deviceInformationLength = genericLength memoryTypes * memoryInformationLength

memoryInformation :: DeviceInformation -> MemoryType -> MemoryInformation
memoryInformation (DeviceInformation memoryInformation) memoryType = fromJust (lookup memoryType (zip memoryTypes memoryInformation))
