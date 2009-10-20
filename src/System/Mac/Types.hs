{-# LANGUAGE ForeignFunctionInterface, GeneralizedNewtypeDeriving #-}

module System.Mac.Types (
  Handle, withHandle,
  OSErr,
  OSType (..),
  OSStatus,
  peekPascalStr
) where

import Control.Exception (bracket)
import Data.Bits (bitSize, shift)
import Data.Char (ord, chr)
import Data.Word (Word8)
import Foreign (Ptr, Storable (..), castPtr)
import Foreign.C.Types (CInt, CShort)
import GHC.Exts (IsString (..))
import System.Mac.OSStatus

newtype OSType = OSType CInt deriving (Eq, Ord, Storable)

instance IsString OSType where
  fromString = OSType . foldl1 (<<) . map (fromIntegral . ord) . pad 4
    where pad n  = take n . (++ repeat ' ')
          x << y = shift x 8 + y

instance Show OSType where
  show (OSType n) = show str
    where str = map (chr . fromIntegral . f) [0..3]
          f i = shift (shift n (i*8)) (-24)

type OSErr = CShort

type Handle = Ptr (Ptr ())

foreign import ccall "NewHandle"
  c_NewHandle :: Int -> IO Handle

foreign import ccall "DisposeHandle"
  c_DisposeHandle :: Handle -> IO ()

withHandle :: (Handle -> IO a) -> IO a
withHandle = bracket
  (c_NewHandle $ bitSize (undefined::CInt) `div` 8)
  c_DisposeHandle

peekPascalStr :: Ptr a -> IO String
peekPascalStr ptr = do
  let ptr' = castPtr ptr :: Ptr Word8
  sz <- fromIntegral `fmap` peek ptr'
  mapM (peekByteOff ptr') [1..sz]
