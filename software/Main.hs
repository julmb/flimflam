import Data.Word
import qualified Data.ByteString.Lazy as BL
import Text.Printf
import System.IO
import System.Environment
import System.Ftdi
import FlimFlam.Memory
import FlimFlam.FirmwareCommand
import FlimFlam.Communication

-- TODO: see what happens when we write to bootloader flash and read it back before and afterwards
-- TODO: see what happens when we read beyond the address space
-- TODO: when writing a program, we should probably pad with 0x0000 (align correctly, maybe reject images with odd size), 0x0000 is nop and will skip to bootloader
-- TODO: should we be able to parse hex? or just add the objcopy stuff to the makefile for easy binary generation?
-- TODO: add flimflam execution to application makefile to automatically flash the MCU
-- TODO: can we use the parser monad Get for something?
-- TODO: can we get rid of most of the bytestring dependencies, or at least the qualified import

data Command = Program | Configure | Dump MemoryType Word16 Word16 | Load MemoryType Word16 | Command FirmwareCommand Integer deriving (Eq, Show, Read)

executeCommand :: Context -> Command -> IO ()
executeCommand context (Dump memoryType position length) = do
	deviceInformation <- getDeviceInformation context
	readData <- readMemory context deviceInformation memoryType position length
	BL.putStr readData
executeCommand context (Load memoryType position) = do
	deviceInformation <- getDeviceInformation context
	writeData <- BL.getContents
	writeMemory context deviceInformation memoryType position (fromIntegral (BL.length writeData)) writeData
executeCommand context (Command command responseLength) = BL.getContents >>= executeFirmwareCommand context command responseLength >>= BL.putStr

-- TODO: add help screen
main :: IO ()
main = do
	let device = Device { vendorID = 0x0403, productID = 0x6001, System.Ftdi.index = 0}
	let parameters = Parameters { baudRate = 20000 }
	hPutStrLn stderr $ printf "parsing parameters..."
	command <- getArgs >>= return . read . unwords
	let run context = do
		hPutStrLn stderr $ printf "executing command..."
		executeCommand context command
	hPutStrLn stderr $ printf "opening FTDI device..."
	withContext device parameters run
