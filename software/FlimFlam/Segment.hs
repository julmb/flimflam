module FlimFlam.Segment (Segment (..), rangeSegment, baseSegment, byteSegment) where

import Numeric.Natural
import Text.Printf
import Linca.Error

data Segment storage = Segment { storage :: storage, offset :: Natural, length :: Natural }

rangeSegment :: storage -> Natural -> Natural -> Segment storage
rangeSegment storage lower upper
	| lower > upper = error $ errorMessage "rangeSegment" $ printf "parameter lower (0x%X) was larger than parameter upper (0x%X)" lower upper
	| otherwise = Segment storage lower (upper - lower)

baseSegment :: storage -> Natural -> Segment storage
baseSegment storage length = Segment storage 0 length

byteSegment :: storage -> Natural -> Segment storage
byteSegment storage position = Segment storage position 1
