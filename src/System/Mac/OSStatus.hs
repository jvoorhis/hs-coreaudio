{-# LANGUAGE DeriveDataTypeable #-}

module System.Mac.OSStatus (
  OSStatus,
  noErr,
  requireNoErr
) where

import Control.Exception (Exception, throwIO)
import Data.Typeable (Typeable)
import Foreign.C.Types (CInt)

type OSStatus = CInt

-- An OSErr may be thrown by requireNoErr when the result of an IO OSStatus
-- action /= noErr
data OSException = OSException !OSStatus String
  deriving (Typeable, Show)

instance Exception OSException

noErr :: OSStatus
noErr = 0

requireNoErr :: IO OSStatus -> IO ()
requireNoErr action = do 
  err <- action
  if err /= noErr
    then throwIO $ OSException err "Unknown error"
    else return ()
{-# INLINE requireNoErr #-}
