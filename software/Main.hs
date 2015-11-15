import Numeric.Natural
import Data.Monoid
import qualified Data.ByteString.Lazy as BL
import Control.Monad
import Text.Printf
import System.Environment
import System.Ftdi
import FlimFlam.Memory
import FlimFlam.FirmwareCommand
import FlimFlam.DeviceInformation
import FlimFlam.Communication

data Command = Run | Information | Program | Configure | Dump MemoryType Natural Natural | Load MemoryType Natural | Command FirmwareCommand Natural deriving (Eq, Show, Read)

executeCommand :: Context -> Command -> IO ()
executeCommand context Run = runApplication context
executeCommand context Information = readDeviceInformation context >>= putStr . show
executeCommand context Program = do
	deviceInformation <- readDeviceInformation context
	let applicationLength = FlimFlam.DeviceInformation.applicationLength deviceInformation
	applicationData <- BL.getContents
	let applicationDataLength = fromIntegral $ BL.length applicationData
	when (applicationDataLength > applicationLength) $
		error $ printf "the application data length (0x%X) was greater than the application length (0x%X)" applicationDataLength applicationLength
	let padding = BL.replicate (fromIntegral (applicationLength - applicationDataLength)) 0x00
	writeVerifyMemory context deviceInformation Flash 0 $ applicationData <> padding
executeCommand context Configure = do
	deviceInformation <- readDeviceInformation context
	let configurationLength = memoryPageCount deviceInformation Eeprom * memoryPageLength deviceInformation Eeprom
	configurationData <- BL.getContents
	let configurationDataLength = fromIntegral $ BL.length configurationData
	when (configurationDataLength > configurationLength) $
		error $ printf "the configuration data length (0x%X) was greater than the configuration length (0x%X)" configurationDataLength configurationLength
	let padding = BL.replicate (fromIntegral (configurationLength - configurationDataLength)) 0x00
	writeVerifyMemory context deviceInformation Eeprom 0 $ configurationData <> padding
executeCommand context (Dump memoryType position length) = do
	deviceInformation <- readDeviceInformation context
	readMemory context deviceInformation memoryType position length >>= BL.putStr
executeCommand context (Load memoryType position) = do
	deviceInformation <- readDeviceInformation context
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
