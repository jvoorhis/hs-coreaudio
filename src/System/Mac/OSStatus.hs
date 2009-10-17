{-# LANGUAGE DeriveDataTypeable #-}

module System.Mac.OSStatus (
  OSStatus,
  OSStatusException,
  noErr,
  requireNoErr
) where

import Control.Exception (Exception, throwIO)
import Data.Typeable (Typeable)
import Foreign.C.Types (CInt)

type OSStatus = CInt

-- | An 'OSStatusException' signals an 'IO' 'OSStatus' action returning a
-- value other than 'noErr'
data OSStatusException = OSStatusException !OSStatus String
  deriving (Typeable, Show)

instance Exception OSStatusException

-- | 'OSStatus' value indicating success
noErr :: OSStatus
noErr = 0

-- | Runs an action of type 'IO' 'OSStatus' and returns 'IO' '()'
-- or throws an 'OSStatusException'.
requireNoErr :: IO OSStatus -> IO ()
requireNoErr action = do 
  err <- action
  if err /= noErr
    then throwIO $ OSStatusException err "Unknown error"
    else return ()
{-# INLINE requireNoErr #-}
