module FlimFlam.Memory (storedMemoryAccess) where

import Numeric.Natural
import Data.Monoid
import qualified Data.ByteString.Lazy as BL
import Text.Printf
import Linca.Size
import FlimFlam.Access
import FlimFlam.Segment

storedMemoryLength :: [Segment storage] -> Natural
storedMemoryLength = size

readStoredMemory :: Monad m => (storage -> StorageAccess m) -> [Segment storage] -> m BL.ByteString
readStoredMemory _ [] = return mempty
readStoredMemory storageAccess (segment : segments) = do
	chunk <- readStorage (storageAccess $ storage segment) (offset segment) (size segment)
	rest <- readStoredMemory storageAccess segments
	return (chunk <> rest)

writeStoredMemory :: Monad m => (storage -> StorageAccess m) -> [Segment storage] -> BL.ByteString -> m ()
writeStoredMemory storageAccess segments writeData
	| fromIntegral (BL.length writeData) /= size segments = error $
		printf "writeStoredMemory: the data length (0x%X) was not equal to the segments length (0x%X)" (BL.length writeData) (size segments)
	| otherwise = go segments writeData
	where
		go [] _ = return ()
		go (segment : segments) writeData = do
			let length = fromIntegral $ size segment
			writeStorage (storageAccess $ storage segment) (offset segment) (BL.take length writeData)
			go segments (BL.drop length writeData)

storedMemoryAccess :: Monad m => (storage -> StorageAccess m) -> [Segment storage] -> MemoryAccess m
storedMemoryAccess storageAccess segments = MemoryAccess (storedMemoryLength segments) (readStoredMemory storageAccess segments) (writeStoredMemory storageAccess segments)
