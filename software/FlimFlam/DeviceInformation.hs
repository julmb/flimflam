module FlimFlam.DeviceInformation where

import Numeric.Natural
import Data.List
import Data.Binary
import Data.Binary.Get
import Text.Printf
import Linca.List
import FlimFlam.Memory

data MemoryInformation = MemoryInformation { pageCount :: Natural, pageLength :: Natural }

instance Show MemoryInformation where
	show memoryInformation = printf "page count = 0x%X, page length = 0x%X" (pageCount memoryInformation) (pageLength memoryInformation)

instance Binary MemoryInformation where
	put = undefined
	get = do
		pageCount <- getWord16le
		pageLength <- getWord16le
		return $ MemoryInformation (fromIntegral pageCount) (fromIntegral pageLength)

memoryInformationLength :: Natural
memoryInformationLength = 2 + 2

data DeviceInformation =
	DeviceInformation
	{
		memoryInformation :: MemoryType -> MemoryInformation,
		signature :: (Word8, Word8, Word8),
		calibration :: Word8,
		lowFuse :: Word8,
		highFuse :: Word8,
		extendedFuse :: Word8,
		lock :: Word8
	}

instance Show DeviceInformation where
	show deviceInformation = unlines (map memoryLine memoryTypes ++ [signatureLine, calibrationLine, fuseLine, lockLine]) where
		memoryLine memoryType = printf "memory %s: %s" (show memoryType) (show (memoryInformation deviceInformation memoryType))
		(signature0, signature1, signature2) = signature deviceInformation
		signatureLine = printf "signature: bytes = 0x%02X 0x%02X 0x%02X" signature0 signature1 signature2
		calibrationLine = printf "calibration: byte = 0x%02X" (calibration deviceInformation)
		fuseLine = printf "fuses: low = 0x%02X, high = 0x%02X, extended = 0x%02X" (lowFuse deviceInformation) (highFuse deviceInformation) (extendedFuse deviceInformation)
		lockLine = printf "lock: byte = 0x%02X" (lock deviceInformation)

instance Binary DeviceInformation where
	put = undefined
	get = do
		let entry memoryType = do
			memoryInformation <- get;
			return (memoryType, memoryInformation)
		table <- traverse entry memoryTypes
		signature0 <- getWord8
		calibration <- getWord8
		signature1 <- getWord8
		skip 1
		signature2 <- getWord8
		skip 11
		lowFuse <- getWord8
		lock <- getWord8
		extendedFuse <- getWord8
		highFuse <- getWord8
		skip 12
		return $ DeviceInformation (retrieve table) (signature0, signature1, signature2) calibration lowFuse highFuse extendedFuse lock

deviceInformationLength :: Natural
deviceInformationLength = genericLength memoryTypes * memoryInformationLength + 0x10 + 0x10

memoryPageCount :: DeviceInformation -> MemoryType -> Natural
memoryPageCount deviceInformation memoryType = pageCount (memoryInformation deviceInformation memoryType)

memoryPageLength :: DeviceInformation -> MemoryType -> Natural
memoryPageLength deviceInformation memoryType = pageLength (memoryInformation deviceInformation memoryType)
