import Data.List
import qualified Data.ByteString.Lazy as BL
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Cont
import System.Environment
import System.FilePath
import System.Directory
import qualified System.Ftdi as Ftdi
import Text.Printf

import FlimFlam.Access
import FlimFlam.Device
import qualified FlimFlam.Devices.ATmega328 as M328

data Configuration = ATmega328Configuration Ftdi.Device Ftdi.Parameters deriving (Show, Read)
data Command memory = Help | Exit | Read memory | Write memory deriving (Show, Read)

execute :: Device memory -> Command memory -> IO ()
execute device Help = do
	putStrLn $ printf "commands: Help | Exit | Read memory | Write memory"
	let memoryEntry memory = printf "%s [0x%X]" (showMemory device memory) (memoryLength $ memoryAccess device memory)
	putStrLn $ printf "memories: %s" (intercalate " | " (memoryEntry <$> memories device))
execute device Exit = exitBootLoader device
execute device (Read memory) = readMemory (memoryAccess device memory) >>= BL.putStr
execute device (Write memory) = do
	let MemoryAccess memoryLength readMemory writeMemory = memoryAccess device memory
	inputData <- BL.getContents
	let inputLength = fromIntegral $ BL.length inputData
	when (inputLength > memoryLength) $ error $ printf "the input length (0x%X) was greater than the memory length (0x%X)" inputLength memoryLength
	let padding = BL.replicate (fromIntegral (memoryLength - inputLength)) 0x00
	let writeData = inputData <> padding
	writeMemory writeData
	readData <- readMemory
	when (readData /= writeData) $ error $ printf "the read data was not equal to the written data"

main :: IO ()
main = evalContT $ do
	homeDirectory <- lift getHomeDirectory
	let path = homeDirectory </> ".config" </> "flimflam"
	configuration <- read <$> lift (readFile path)
	device <- case configuration of
		ATmega328Configuration device parameters -> M328.withDevice device parameters
	command <- read <$> unwords <$> lift getArgs
	lift $ execute device command
