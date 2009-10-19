{-# LANGUAGE GeneralizedNewtypeDeriving, PatternGuards #-}

-- | A 'Storable' wrapper for @OSType@, a FourCC type common in Apple APIs.
-- @System.Mac.OSType@ is meant to be used with @-XOverloadedStrings@.
module System.Mac.OSType (
  OSType (..)
) where

import Data.Bits (shift)
import Data.Char (ord, chr)
import Foreign (Storable)
import Foreign.C.Types (CInt)
import GHC.Exts (IsString (..))

newtype OSType = OSType CInt deriving (Eq, Ord, Storable)

pad :: Int -> String -> String
pad n = take n . (++ repeat ' ')

instance IsString OSType where
  fromString = OSType . foldl1 (<<) . map (fromIntegral . ord) . pad 4
    where x << y = shift x 8 + y

instance Show OSType where
  show (OSType n) = show str
    where str = map (chr . fromIntegral . f) [0..3]
          f i = shift (shift n (i*8)) (-24)
