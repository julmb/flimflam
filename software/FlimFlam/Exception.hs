module FlimFlam.Exception (FlimFlamException (..)) where

import Data.Typeable
import Control.Exception
import Text.Printf
import FlimFlam.FirmwareCommand

data FlimFlamException = InputException String String | ResponseException FirmwareCommand String | VerificationException deriving Typeable

instance Show FlimFlamException where
	show (InputException location message) = printf "input exception %s: %s" location message
	show (ResponseException command message) = printf "response exception %s: %s" (show command) message
	show VerificationException = printf "verification exception: the written data was different from the read data"
instance Exception FlimFlamException
