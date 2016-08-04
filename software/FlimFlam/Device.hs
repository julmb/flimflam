module FlimFlam.Device (Device (..)) where

import FlimFlam.Access

data Device memory =
	Device
	{
		memories :: [memory],
		showMemory :: memory -> String,
		parseMemory :: String -> memory,
		exitBootLoader :: IO (),
		memoryAccess :: memory -> MemoryAccess IO
	}
