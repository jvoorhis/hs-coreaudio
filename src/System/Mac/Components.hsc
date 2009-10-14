{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module System.Mac.Components (
  ComponentDescription (..),
  ComponentInstance
) where

import Foreign
import Foreign.C.Types
import System.Mac.OSType

#include "/System/Library/Frameworks/CoreServices.framework/Frameworks/CarbonCore.framework/Headers/Components.h"
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

data ComponentDescription = ComponentDescription {
  componentType         :: OSType,
  componentSubType      :: OSType,
  componentManufacturer :: OSType,
  componentFlags        :: CUInt,
  componentFlagsMask    :: CUInt
}

data ComponentInstanceRecord
type ComponentInstance = Ptr ComponentInstanceRecord

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
