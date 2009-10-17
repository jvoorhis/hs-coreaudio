{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface, GeneralizedNewtypeDeriving #-}

-- | Interface to the OS X Component Manager. Types are Storable
-- instances corresponding to the C types documented at
-- <http://developer.apple.com/mac/library/documentation/Carbon/Reference/Component_Manager/Reference/reference.html>.

module System.Mac.Components (
  ComponentDescription (..),
  ComponentInstance,
  ComponentResult
) where

import Foreign (Ptr, Storable (..))
import Foreign.C.Types (CInt, CUInt)
import System.Mac.OSType

#include "/System/Library/Frameworks/CoreServices.framework/Frameworks/CarbonCore.framework/Headers/Components.h"
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

data ComponentDescription = ComponentDescription {
  componentType         :: OSType,
  componentSubType      :: OSType,
  componentManufacturer :: OSType,
  componentFlags        :: CUInt,
  componentFlagsMask    :: CUInt
} deriving (Eq, Show)

instance Storable ComponentDescription where
  alignment _ = #{alignment ComponentDescription}
  sizeOf _    = #{size ComponentDescription}
  peek ptr    = do
    t <- #{peek ComponentDescription, componentType} ptr
    s <- #{peek ComponentDescription, componentSubType} ptr
    m <- #{peek ComponentDescription, componentManufacturer} ptr
    f <- #{peek ComponentDescription, componentFlags} ptr
    k <- #{peek ComponentDescription, componentFlagsMask} ptr
    return $ ComponentDescription t s m f k
  poke ptr (ComponentDescription t s m f k) = do
    #{poke ComponentDescription, componentType} ptr t
    #{poke ComponentDescription, componentSubType} ptr s
    #{poke ComponentDescription, componentManufacturer} ptr m
    #{poke ComponentDescription, componentFlags} ptr f
    #{poke ComponentDescription, componentFlagsMask} ptr k

data ComponentInstanceRecord

type ComponentInstance = Ptr ComponentInstanceRecord

type ComponentResult = CInt
