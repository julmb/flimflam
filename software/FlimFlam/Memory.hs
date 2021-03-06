module FlimFlam.Memory (storedMemoryAccess) where

import Numeric.Natural
import qualified Data.ByteString.Lazy as BL
import Text.Printf
import Linca.Error

import FlimFlam.Access
import FlimFlam.Segment

storedMemoryLength :: [Segment storage] -> Natural
storedMemoryLength = sum . map FlimFlam.Segment.length

readStoredMemory :: Monad m => (storage -> StorageAccess m) -> [Segment storage] -> m BL.ByteString
readStoredMemory _ [] = return mempty
readStoredMemory storageAccess (Segment storage offset length : segments) = do
	chunk <- readStorage (storageAccess storage) offset length
	rest <- readStoredMemory storageAccess segments
	return (chunk <> rest)

writeStoredMemory :: Monad m => (storage -> StorageAccess m) -> [Segment storage] -> BL.ByteString -> m ()
writeStoredMemory _ [] _ = return ()
writeStoredMemory storageAccess (Segment storage offset length : segments) writeData = do
	writeStorage (storageAccess storage) offset (BL.take (fromIntegral length) writeData)
	writeStoredMemory storageAccess segments (BL.drop (fromIntegral length) writeData)

-- TODO: this is a really strange way of error handling, checkSegment should be in the actual guard and checkWriteStoredMemory should be in the function (which can currently be used with incorrect parameters)
storedMemoryAccess :: Monad m => (storage -> StorageAccess m) -> [Segment storage] -> MemoryAccess m
storedMemoryAccess storageAccess segments
	| any (checkSegment storageAccess) segments = undefined
	| otherwise = MemoryAccess (storedMemoryLength segments) (readStoredMemory storageAccess segments) (checkWriteStoredMemory storageAccess segments)
	where
		checkSegment storageAccess (Segment storage offset length)
			| offset + length > storageLength (storageAccess storage) = error $ errorMessage "storedMemoryAccess" $ printf
				"offset + length (0x%X) was larger than the storage length (0x%X)" (offset + length) (storageLength $ storageAccess storage)
			| otherwise = False
		checkWriteStoredMemory storageAccess segments writeData
			| fromIntegral (BL.length writeData) /= storedMemoryLength segments = error $ errorMessage "writeStoredMemory" $ printf
				"the data length (0x%X) was not equal to the segments length (0x%X)" (BL.length writeData) (storedMemoryLength segments)
			| otherwise = writeStoredMemory storageAccess segments writeData
