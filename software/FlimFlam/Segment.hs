module FlimFlam.Segment
(
	Segment, segment, storage, offset, FlimFlam.Segment.length,
	rangeSegment, baseSegment, byteSegment, byteSegments
)
where

import Numeric.Natural
import Text.Printf
import Linca.Size

data Segment storage = Segment { storage :: storage, offset :: Natural, length :: Natural } deriving (Eq, Show, Read)

instance Size (Segment storage) where
	size = FlimFlam.Segment.length

segment :: Size storage => storage -> Natural -> Natural -> Segment storage
segment storage offset length
	| offset + length > size storage = error $ printf "segment: offset + length (0x%X) was larger than the storage length (0x%X)" (offset + length) (size storage)
	| otherwise = Segment storage offset length

rangeSegment :: Size storage => storage -> Natural -> Natural -> Segment storage
rangeSegment storage lower upper
	| lower > upper = error $ printf "rangeSegment: parameter lower (0x%X) was larger than parameter upper (0x%X)" lower upper
	| otherwise = segment storage lower (upper - lower)

baseSegment :: Size storage => storage -> Natural -> Segment storage
baseSegment storage = segment storage 0

byteSegment :: Size storage => storage -> Natural -> Segment storage
byteSegment storage position = segment storage position 1

byteSegments :: Size storage => storage -> [Natural] -> [Segment storage]
byteSegments = map . byteSegment
