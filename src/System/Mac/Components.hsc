{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface, GeneralizedNewtypeDeriving #-}

-- | Interface to the OS X Component Manager. Types are Storable
-- instances corresponding to the C types documented at
-- <http://developer.apple.com/mac/library/documentation/Carbon/Reference/Component_Manager/Reference/reference.html>.

module System.Mac.Components (
  ComponentDescription (..),
  Component,
  ComponentInstance,
  ComponentResult,
  any,
  noComponent,
  countComponents,
  findNextComponent,
  components,
  componentDescription,
  componentName
) where

import Prelude hiding (any)
import Control.Applicative
import Control.Exception (bracket)
import Data.Bits (bitSize)
import Foreign (Ptr, Storable (..), alloca, nullPtr)
import Foreign.C.Types (CInt, CUInt, CLong)
import System.Mac.OSType
import System.Mac.Types

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

data ComponentRecord

type Component = Ptr ComponentRecord

data ComponentInstanceRecord

type ComponentInstance = Ptr ComponentInstanceRecord

type ComponentResult = CInt

-- | When searching for components by a ComponentDescription, setting a field's
-- value to 'any' causes the component manager to ignore that field.
any :: OSType
any = OSType 0

noComponent :: Component
noComponent = nullPtr

foreign import ccall "Components.h CountComponents"
  c_CountComponents :: Ptr ComponentDescription -> IO CLong

-- | Count the number of components matching a given 'ComponentDescription'.
-- Define any 'ComponentDescription' field as 'any' to ignore it.
countComponents :: ComponentDescription -> IO Int
countComponents desc = alloca $ \ptr -> do
  poke ptr desc
  fromIntegral <$> c_CountComponents ptr
{-# INLINE countComponents #-}

foreign import ccall "Components.h FindNextComponent"
  c_FindNextComponent :: Component -> Ptr ComponentDescription -> IO Component

findNextComponent :: Component -> ComponentDescription -> IO (Maybe Component)
findNextComponent component looking = alloca $ \ptr -> do
  poke ptr looking
  box <$> c_FindNextComponent component ptr
  where box found | found == noComponent = Nothing
                  | otherwise = Just found
{-# INLINE findNextComponent #-}

-- | Find all 'Component's matching a given description
components :: ComponentDescription -> IO [Component]
components desc = search noComponent []
    where search c cs = do
            out <- findNextComponent c desc
            case out of
              Just found -> search found $ cs ++ [found]
              Nothing    -> return cs

foreign import ccall "Components.h GetComponentInfo"
  c_GetComponentInfo :: Component -> Ptr ComponentDescription
                     -> Handle -> Handle -> Handle
                     -> IO OSErr

-- | Call @GetComponentInfo@ to retrieve a 'Component''s ComponentDescription
componentDescription :: Component -> IO ComponentDescription
componentDescription component = alloca $ \ptr -> do
  c_GetComponentInfo component ptr nullPtr nullPtr nullPtr
  peek ptr
{-# INLINE componentDescription #-}

-- | Call @GetComponentInfo@ to retrieve a 'Component''s name as a String
componentName :: Component -> IO String
componentName component = bracket
  (c_NewHandle $ bitSize (undefined::CInt) `div` 8)
  c_DisposeHandle
  (\h -> do
    c_GetComponentInfo component nullPtr h nullPtr nullPtr
    peek h >>= peekPascalStr)
{-# INLINE componentName #-}
