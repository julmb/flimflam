module FlimFlam.Exception where

import Data.Typeable
import Control.Exception
import Text.Printf
import FlimFlam.FirmwareCommand

data FlimFlamException = ResponseException FirmwareCommand String deriving Typeable

instance Show FlimFlamException where
	show (ResponseException command message) = printf "response exception %s: %s" (show command) message
instance Exception FlimFlamException
