module FlimFlam.Paging (pagedStorageAccess) where

import Numeric.Natural
import Linca.Scalar
import Data.Monoid
import qualified Data.ByteString.Lazy as BL
import qualified Linca.ByteString.Lazy as BL
import Text.Printf
import Linca.Error

import FlimFlam.Access

pagedStorageLength :: PagingAccess m -> Natural
pagedStorageLength pagingAccess = pageCount (pagingLength pagingAccess) * pageLength (pagingLength pagingAccess)

readPagedStorage :: Monad m => PagingAccess m -> Natural -> Natural -> m BL.ByteString
readPagedStorage pagingAccess dataOffset dataLength
	| dataOffset + dataLength > pagedStorageLength pagingAccess = error $ errorMessage "readPagedStorage" $ printf
		"dataOffset + dataLength (0x%X) was greater than the storage length (0x%X)" (dataOffset + dataLength) (pagedStorageLength pagingAccess)
	| dataLength == 0 = return mempty
	| otherwise = do
		chunk <- readPage pagingAccess pageIndex >>= return . BL.take (fromIntegral chunkLength) . BL.drop (fromIntegral pageOffset)
		rest <- readPagedStorage pagingAccess (dataOffset + chunkLength) (dataLength - chunkLength)
		return (chunk <> rest)
	where
		(pageIndex, pageOffset) = normalize (pageLength (pagingLength pagingAccess)) (0, dataOffset)
		chunkLength = min dataLength (pageLength (pagingLength pagingAccess) - pageOffset)

writePagedStorage :: Monad m => PagingAccess m -> Natural -> BL.ByteString -> m ()
writePagedStorage pagingAccess dataOffset writeData
	| dataOffset + dataLength > pagedStorageLength pagingAccess = error $ errorMessage "writePagedStorage" $ printf
		"dataOffset + dataLength (0x%X) was greater than the storage length (0x%X)" (dataOffset + dataLength) (pagedStorageLength pagingAccess)
	| dataLength == 0 = return ()
	| chunkLength == pageLength (pagingLength pagingAccess) = do
		writePage pagingAccess pageIndex chunk
		writePagedStorage pagingAccess (dataOffset + chunkLength) rest
	| otherwise = do
		readPage pagingAccess pageIndex >>= return . BL.replace pageOffset chunk >>= writePage pagingAccess pageIndex
		writePagedStorage pagingAccess (dataOffset + chunkLength) rest
	where
		dataLength = fromIntegral (BL.length writeData)
		(pageIndex, pageOffset) = normalize (pageLength (pagingLength pagingAccess)) (0, dataOffset)
		chunkLength = min dataLength (pageLength (pagingLength pagingAccess) - pageOffset)
		chunk = BL.take (fromIntegral chunkLength) writeData
		rest = BL.drop (fromIntegral chunkLength) writeData

pagedStorageAccess :: Monad m => PagingAccess m -> StorageAccess m
pagedStorageAccess pagingAccess = StorageAccess (pagedStorageLength pagingAccess) (readPagedStorage pagingAccess) (writePagedStorage pagingAccess)
