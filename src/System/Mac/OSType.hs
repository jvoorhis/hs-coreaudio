{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Mac.OSType (
  OSType (..),
  fromString
) where

import Data.Bits (shift)
import Data.Char (ord)
import Foreign (Storable)
import Foreign.C.Types (CInt)
import GHC.Exts (IsString (..))

newtype OSType = OSType CInt deriving (Show, Eq, Ord, Storable)

pad :: Int -> String -> String
pad n = take n . (++ repeat ' ')

instance IsString OSType where
  fromString = OSType . foldl1 (<<) . map (fromIntegral . ord) . pad 4
    where x << y = shift x 8 + y
