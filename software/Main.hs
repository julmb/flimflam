import Data.List
import Data.Monoid
import qualified Data.ByteString.Lazy as BL
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Cont
import System.Environment
import Text.Printf

import FlimFlam.Access
import FlimFlam.Device
import qualified FlimFlam.Devices.ATmega328 as M328

data Command memory = Help | Exit | Read memory | Write memory deriving (Show, Read)

execute :: Device memory -> Command memory -> IO ()
execute device Help = do
	putStrLn $ printf "commands: Help | Run | Read memory | Write memory"
	let memoryEntry memory = printf "%s [0x%X]" (showMemory device memory) (memoryLength $ memoryAccess device memory)
	putStrLn $ printf "memories: %s" (intercalate " | " (memoryEntry <$> memories device))
execute device Run = runApplication device
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
	device <- M328.withDevice
	command <- read <$> unwords <$> lift getArgs
	lift $ execute device command
