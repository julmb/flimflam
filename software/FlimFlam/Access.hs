module FlimFlam.Access (PagingLength (..), pagingTotalLength, PagingAccess (..), StorageAccess (..), MemoryAccess (..)) where

import Numeric.Natural
import qualified Data.ByteString.Lazy as BL

data PagingLength =
	PagingLength
	{
		pageCount :: Natural,
		pageLength :: Natural
	}

pagingTotalLength :: PagingLength -> Natural
pagingTotalLength pagingLength = pageCount pagingLength * pageLength pagingLength

data PagingAccess m =
	PagingAccess
	{
		pagingLength :: PagingLength,
		readPage :: Natural -> m BL.ByteString,
		writePage :: Natural -> BL.ByteString -> m ()
	}

data StorageAccess m =
	StorageAccess
	{
		storageLength :: Natural,
		readStorage :: Natural -> Natural -> m BL.ByteString,
		writeStorage :: Natural -> BL.ByteString -> m ()
	}

data MemoryAccess m =
	MemoryAccess
	{
		memoryLength :: Natural,
		readMemory :: m BL.ByteString,
		writeMemory :: BL.ByteString -> m ()
	}
