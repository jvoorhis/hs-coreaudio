{-# LANGUAGE DeriveDataTypeable #-}

module System.Mac.OSStatus (
  OSStatus,
  noErr,
  requireNoErr
) where

import Control.Exception (Exception, throwIO)
import Data.Typeable
import Foreign.C.Types (CInt)

type OSStatus = CInt

-- An OSErr may be thrown by requireNoErr when the result of an IO OSStatus
-- action /= noErr
data OSErr = OSErr !OSStatus String
  deriving (Typeable, Show)

instance Exception OSErr

noErr :: OSStatus
noErr = 0

requireNoErr :: IO OSStatus -> IO OSStatus
requireNoErr action = do 
  err <- action
  if err /= noErr
    then throwIO $ OSErr err ""
    else return err
{-# INLINE requireNoErr #-}
