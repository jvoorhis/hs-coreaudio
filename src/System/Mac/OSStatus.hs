{-# LANGUAGE DeriveDataTypeable #-}

module System.Mac.OSStatus (
  OSStatus,
  noErr,
  requireNoErr
) where

import Control.Exception
import Data.Typeable
import Foreign.C.Types (CInt)

type OSStatus = CInt

data OSErr = OSErr !OSStatus String
  deriving (Typeable, Show)

instance Exception OSErr

noErr :: OSStatus
noErr = 0

requireNoErr :: IO OSStatus -> IO OSStatus
requireNoErr action = do 
  err <- action
  if noErr == err then return err
    else throw $ OSErr err ""
{-# INLINE requireNoErr #-}
