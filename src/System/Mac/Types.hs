{-# LANGUAGE ForeignFunctionInterface #-}

module System.Mac.Types where

import Data.Word (Word8)
import Foreign (Ptr, Storable (..), castPtr)
import Foreign.C.Types (CShort)

type OSErr = CShort

type Handle = Ptr (Ptr ())

foreign import ccall "NewHandle"
  c_NewHandle :: Int -> IO Handle

foreign import ccall "DisposeHandle"
  c_DisposeHandle :: Handle -> IO ()

peekPascalStr :: Ptr a -> IO String
peekPascalStr ptr = do
  let ptr' = castPtr ptr :: Ptr Word8
  sz <- fromIntegral `fmap` peek ptr'
  mapM (peekByteOff ptr') [1..sz]
