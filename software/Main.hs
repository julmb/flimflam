import Numeric.Natural
import qualified Data.ByteString.Lazy as BL
import System.Environment
import System.Ftdi
import FlimFlam.Memory
import FlimFlam.FirmwareCommand
import FlimFlam.Communication

-- TODO: when writing a program, we should probably pad with 0x0000 (align correctly, maybe reject images with odd size), 0x0000 is nop and will skip to bootloader
-- TODO: should we be able to parse hex? or just add the objcopy stuff to the makefile for easy binary generation?
-- TODO: add flimflam execution to application makefile to automatically flash the MCU
-- TODO: add verification after writing memory

data Command = Program | Configure | Dump MemoryType Natural Natural | Load MemoryType Natural | Command FirmwareCommand Natural | DeviceInformation deriving (Eq, Show, Read)

executeCommand :: Context -> Command -> IO ()
executeCommand context (Dump memoryType position length) = do
	deviceInformation <- getDeviceInformation context
	readMemory context deviceInformation memoryType position length >>= BL.putStr
executeCommand context (Load memoryType position) = do
	deviceInformation <- getDeviceInformation context
	BL.getContents >>= writeMemory context deviceInformation memoryType position
executeCommand context (Command command responseLength) = BL.getContents >>= executeFirmwareCommand context command responseLength >>= BL.putStr
executeCommand context DeviceInformation = getDeviceInformation context >>= putStr . show

-- TODO: add help screen
-- TODO: pass device index, or even enable listing devices? put that into ftdihs?
main :: IO ()
main = do
	let device = Device { vendorID = 0x0403, productID = 0x6001, System.Ftdi.index = 0}
	let parameters = Parameters { baudRate = 20000 }
	command <- getArgs >>= return . read . unwords
	let run context = executeCommand context command
	withContext device parameters run
