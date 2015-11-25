import qualified Data.ByteString.Lazy as BL
import System.Environment
import qualified System.Ftdi as Ftdi
import FlimFlam.Access
import FlimFlam.Device
import qualified FlimFlam.Devices.ATmega328 as M328

-- TODO: make Controller a library
-- TODO: adjust firmware to loader utility in naming and architecture
-- TODO: organize directory structure to reflect genericity
-- TODO: make no hard-coded commands, just execute "program" on the "application" partition
-- TODO: shrink command-line interface to only the necessary basics
-- TODO: what about writing to read-only memory?

{-
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

main :: IO ()
main = do
	let device = Device { vendorID = 0x0403, productID = 0x6001, System.Ftdi.index = 0}
	let parameters = Parameters { baudRate = 20000 }
	command <- getArgs >>= return . read . unwords
	let run context = executeCommand context command
	withContext device parameters run
-}

data Command memory = Help | Run | Dump memory | Load memory deriving (Eq, Show, Read)

-- TODO: display available commands, partition table, memory and storage sizes, etc. (basically all the available device info in the device specification module)
-- TODO: or maybe we just want to have the information that's relevant w.r.t. command-line parameters? in which case everything should be available through device anyways?
executeCommand :: Device memory -> Command memory -> IO ()
executeCommand device Help = putStr (unlines (map (showMemory device) (memories device)))
executeCommand device Run = runApplication device
executeCommand device (Dump memory) = readMemory (memoryAccess device memory) >>= BL.putStr

-- TODO: pass device index, or even enable listing devices? put that into ftdihs?
-- TODO: maybe we can select the device via command-line parameter?
main :: IO ()
main = do
	let device = Ftdi.Device { Ftdi.vendorID = 0x0403, Ftdi.productID = 0x6001, Ftdi.index = 0}
	let parameters = Ftdi.Parameters { Ftdi.baudRate = 20000 }
	command <- getArgs >>= return . read . unwords
	let run context = executeCommand (M328.device context) command
	Ftdi.withContext device parameters run
