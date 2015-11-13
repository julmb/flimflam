import Numeric.Natural
import Data.Monoid
import qualified Data.ByteString.Lazy as BL
import Control.Monad
import Text.Printf
import System.Environment
import System.Ftdi
import FlimFlam.Memory
import FlimFlam.FirmwareCommand
import qualified FlimFlam.DeviceInformation as DI
import FlimFlam.Communication

-- TODO: when writing a program, we should probably pad with 0x0000 (align correctly, maybe reject images with odd size), 0x0000 is nop and will skip to bootloader
-- TODO: should we be able to parse hex? or just add the objcopy stuff to the makefile for easy binary generation?
-- TODO: add flimflam execution to application makefile to automatically flash the MCU
-- TODO: add writeVerify function to use for the high-level commands Program and Configure
-- TODO: resolve various name conflicts
-- TODO: make naming more consistent
-- TODO: does it make sense to extract the multiplication of pageCount and pageLength to get the memoryLength?

data Command = Program | Configure | DeviceInformation | Dump MemoryType Natural Natural | Load MemoryType Natural | Command FirmwareCommand Natural deriving (Eq, Show, Read)

executeCommand :: Context -> Command -> IO ()
executeCommand context Program = do
	deviceInformation <- getDeviceInformation context
	let programLength = DI.programLength deviceInformation
	programData <- BL.getContents
	let programDataLength = fromIntegral $ BL.length programData
	when (programDataLength > programLength) $ error $ printf "the program data length (0x%X) was greater than the program length (0x%X)" programDataLength programLength
	let padding = BL.replicate (fromIntegral (programLength - programDataLength)) 0x00
	writeMemory context deviceInformation Flash 0 $ programData <> padding
executeCommand context Configure = do
	deviceInformation <- getDeviceInformation context
	let configurationLength = DI.memoryPageCount deviceInformation Eeprom * DI.memoryPageLength deviceInformation Eeprom
	configurationData <- BL.getContents
	let configurationDataLength = fromIntegral $ BL.length configurationData
	when (configurationDataLength > configurationLength) $ error $ printf "the configuration data length (0x%X) was greater than the configuration length (0x%X)" configurationDataLength configurationLength
	let padding = BL.replicate (fromIntegral (configurationLength - configurationDataLength)) 0x00
	writeMemory context deviceInformation Eeprom 0 $ configurationData <> padding
executeCommand context DeviceInformation = getDeviceInformation context >>= putStr . show
executeCommand context (Dump memoryType position length) = do
	deviceInformation <- getDeviceInformation context
	readMemory context deviceInformation memoryType position length >>= BL.putStr
executeCommand context (Load memoryType position) = do
	deviceInformation <- getDeviceInformation context
	BL.getContents >>= writeMemory context deviceInformation memoryType position
executeCommand context (Command command responseLength) = BL.getContents >>= executeFirmwareCommand context command responseLength >>= BL.putStr

-- TODO: add help screen
-- TODO: pass device index, or even enable listing devices? put that into ftdihs?
main :: IO ()
main = do
	let device = Device { vendorID = 0x0403, productID = 0x6001, System.Ftdi.index = 0}
	let parameters = Parameters { baudRate = 20000 }
	command <- getArgs >>= return . read . unwords
	let run context = executeCommand context command
	withContext device parameters run
