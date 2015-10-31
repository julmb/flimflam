import Data.List
import Data.Monoid
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Builder
import Control.Monad
import System.Environment
import System.Ftdi

data Memory = Flash | Eeprom deriving (Eq, Show, Read)
data Storage = Storage Memory Word16 Word16 deriving (Eq, Show, Read)
data Command = Information | Read Storage | Write Storage deriving (Eq, Show, Read)

buildMemory :: Memory -> Builder
buildMemory Flash = word16LE 0x0001
buildMemory Eeprom = word16LE 0x0002

buildStorage :: Storage -> Builder
buildStorage (Storage memory address length) = buildMemory memory <> word16LE address <> word16LE length

buildCommand :: Command -> Builder
buildCommand Information = word16LE 0x0001
buildCommand (Read storage) = word16LE 0x0002 <> buildStorage storage
buildCommand (Write storage) = word16LE 0x0003 <> buildStorage storage

responseLengthCommand :: Command -> Integer
responseLengthCommand Information = 0x0001
responseLengthCommand (Read (Storage memory address length)) = fromIntegral length
responseLengthCommand (Write storage) = 0x0000

execute :: Context -> Command -> IO ()
execute context command = do
	send context (toLazyByteString (buildCommand command))
	result <- receive context (responseLengthCommand command)
	print (BL.unpack result)

run :: Context -> IO ()
run context = do
	parameters <- getArgs
	when (genericLength parameters /= 1) $ error "number of parameters was not 1"
	let command = read (genericIndex parameters 0)
	execute context command

main :: IO ()
main = withContext
	Device { vendorID = 0x0403, productID = 0x6001, System.Ftdi.index = 0}
	Parameters { baudRate = 20000 }
	run
