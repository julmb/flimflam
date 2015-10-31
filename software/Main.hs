import Data.List
import Data.Monoid
import qualified Data.ByteString as BS
import Control.Monad
import System.Environment
import System.Ftdi

terminal :: Context -> IO ()
terminal context = do
	parameters <- getArgs
	let command = read (genericIndex parameters 0)
	let responseLength = read (genericIndex parameters 1)
	sendData context (BS.pack command)
	result <- receiveData context responseLength
	putStrLn (show (BS.unpack result))

main :: IO ()
main = withContext
	Device { vendorID = 0x0403, productID = 0x6001, index = 0}
	Parameters { baudRate = 20000 }
	terminal
