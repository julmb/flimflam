import Data.List
import Data.Monoid
import qualified Data.ByteString.Lazy as BL
import Control.Monad
import System.Environment
import Text.Printf
import qualified System.Ftdi as Ftdi

import FlimFlam.Access
import FlimFlam.Device
import qualified FlimFlam.Devices.ATmega328 as M328

-- TODO: adjust firmware to loader utility in naming and architecture
-- TODO: organize directory structure to reflect genericity

data Command memory = Help | Run | Read memory | Write memory deriving (Show, Read)

executeCommand :: Device memory -> Command memory -> IO ()
executeCommand device Help = do
	putStrLn $ printf "commands: Help | Run | Read memory | Write memory"
	let memoryEntry memory = printf "%s [0x%X]" (showMemory device memory) (memoryLength $ memoryAccess device memory)
	putStrLn $ printf "memories: %s" (intercalate " | " (map memoryEntry (memories device)))
executeCommand device Run = runApplication device
executeCommand device (Read memory) = readMemory (memoryAccess device memory) >>= BL.putStr
executeCommand device (Write memory) = do
	let MemoryAccess memoryLength readMemory writeMemory = memoryAccess device memory
	inputData <- BL.getContents
	let inputLength = fromIntegral $ BL.length inputData
	when (inputLength > memoryLength) $ error $ printf "the input length (0x%X) was greater than the memory length (0x%X)" inputLength memoryLength
	let padding = BL.replicate (fromIntegral (memoryLength - inputLength)) 0x00
	let writeData = inputData <> padding
	writeMemory writeData
	readData <- readMemory
	when (readData /= writeData) $ error $ printf "the read data was not equal to the written data"

-- TODO: pass device index, or even enable listing devices? put that into ftdihs?
-- TODO: maybe we can select both devices via command-line parameter?
main :: IO ()
main = do
	let device = Ftdi.Device { Ftdi.vendorID = 0x0403, Ftdi.productID = 0x6001, Ftdi.index = 0}
	let parameters = Ftdi.Parameters { Ftdi.baudRate = 20000 }
	command <- getArgs >>= return . read . unwords
	let run context = executeCommand (M328.device context) command
	Ftdi.withContext device parameters run
