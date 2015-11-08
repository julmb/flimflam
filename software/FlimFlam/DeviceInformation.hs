module FlimFlam.DeviceInformation where

import Data.Maybe
import Data.List
import Data.Binary
import Data.Binary.Get
import FlimFlam.Memory

data MemoryInformation = MemoryInformation { memoryPageCount :: Integer, memoryPageLength :: Integer } deriving (Eq, Show, Read)

instance Binary MemoryInformation where
	put = undefined
	get = do
		pageCount <- getWord16le
		pageLength <- getWord16le
		return (MemoryInformation (fromIntegral pageCount) (fromIntegral pageLength))

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
