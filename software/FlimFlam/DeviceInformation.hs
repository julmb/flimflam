module FlimFlam.DeviceInformation where

import Numeric.Natural
import Data.Maybe
import Data.List
import Data.Binary
import Data.Binary.Get
import FlimFlam.Memory

data MemoryInformation = MemoryInformation { memoryPageCount :: Natural, memoryPageLength :: Natural } deriving (Eq, Show, Read)

instance Binary MemoryInformation where
	put = undefined
	get = do
		pageCount <- getWord16le
		pageLength <- getWord16le
		return $ MemoryInformation (fromIntegral pageCount) (fromIntegral pageLength)

memoryInformationLength :: Natural
memoryInformationLength = 2 + 2

newtype DeviceInformation = DeviceInformation [MemoryInformation] deriving (Eq, Show, Read)

instance Binary DeviceInformation where
	put = undefined
	get = traverse (const get) memoryTypes >>= return . DeviceInformation

deviceInformationLength :: Natural
deviceInformationLength = genericLength memoryTypes * memoryInformationLength

memoryInformation :: DeviceInformation -> MemoryType -> MemoryInformation
memoryInformation (DeviceInformation memoryInformation) memoryType = fromJust (lookup memoryType (zip memoryTypes memoryInformation))
