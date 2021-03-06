{-# LANGUAGE ForeignFunctionInterface #-}

-- | Module containing types corresponding to the C types defined in
-- @AUComponent.h@ in the @AudioUnit.framework@.

module Sound.AudioUnit.AUComponent (
  AudioUnit,
  PropertyID,
  ParameterID,
  Scope,
  Element,
  ParameterValue,
  getPropertyInfo,
  getProperty,
  getPropertyList,
  getParameter,
  setParameter
) where

import Control.Applicative
import Control.Monad
import Foreign
import Foreign.C.Types (CUInt, CFloat)
import System.Mac.Components (ComponentInstance, ComponentResult)

type AudioUnit = ComponentInstance

type PropertyID = CUInt
type ParameterID = CUInt
type Scope = CUInt
type Element = CUInt
type ParameterValue = CFloat

foreign import ccall "AUComponent.h AudioUnitGetPropertyInfo"
  c_AudioUnitGetPropertyInfo :: AudioUnit -> PropertyID -> Scope -> Element
                             -> Ptr CUInt -> Ptr Bool
                             -> IO ComponentResult

-- Obtain size and read/write access for an AudioUnit property
getPropertyInfo :: AudioUnit
                -> PropertyID -> Scope -> Element
                -> IO (Int, Bool)
getPropertyInfo au prp scp elt =
  alloca $ \szP -> do
  alloca $ \wrP -> do
    c_AudioUnitGetPropertyInfo au prp scp elt szP wrP
    (,) <$> (fromIntegral <$> peek szP) <*> peek wrP
{-# INLINE getPropertyInfo #-}

foreign import ccall "AUComponent.h AudioUnitGetProperty"
  c_AudioUnitGetProperty :: AudioUnit -> PropertyID -> Scope -> Element
                         -> Ptr () -> Ptr CUInt
                         -> IO ComponentResult

-- | Read a scalar property value
getProperty :: Storable a
            => AudioUnit -> PropertyID -> Scope -> Element
            -> IO a
getProperty au prp scp elt = get undefined where
  get :: Storable a => a -> IO a
  get a = let size = sizeOf a
          in allocaBytes size $ \outP -> do
            alloca $ \szP -> do
            poke szP $ toEnum size
            c_AudioUnitGetProperty au prp scp elt (castPtr outP) szP
            peek outP
{-# INLINE getProperty #-}

-- | Read a 'List' of property values
getPropertyList :: Storable a
                => AudioUnit -> PropertyID -> Scope -> Element
                -> IO [a]
getPropertyList au prp scp elt = get undefined where
  get :: Storable a => a -> IO [a]
  get a = alloca $ \szP -> do
    c_AudioUnitGetPropertyInfo au prp scp elt szP nullPtr
    n <- fromIntegral <$> peek szP
    let size  = sizeOf a
        width = size * n
    allocaBytes width $ \outP -> do
    poke szP $ toEnum width
    c_AudioUnitGetProperty au prp scp elt (castPtr outP) szP
    n' <- (`div` size) . fromIntegral <$> peek szP
    forM (take n' [0..]) (peekByteOff outP . (*size))
{-# INLINE getPropertyList #-}

foreign import ccall "AUComponent.h AudioUnitGetParameter"
  c_AudioUnitGetParameter :: AudioUnit -> ParameterID -> Scope -> Element
                          -> Ptr ParameterValue
                          -> IO ComponentResult

getParameter :: AudioUnit -> ParameterID -> Scope -> Element
             -> IO ParameterValue
getParameter au pid scp elt =
  alloca $ \vP -> do
  c_AudioUnitGetParameter au pid scp elt vP
  peek vP
{-# INLINE getParameter #-}

foreign import ccall "AUComponent.h AudioUnitSetParameter"
  c_AudioUnitSetParameter :: AudioUnit -> ParameterID -> Scope -> Element
                          -> ParameterValue -> CUInt
                          -> IO ComponentResult

setParameter :: AudioUnit -> ParameterID -> Scope -> Element
             -> ParameterValue -> Int
             -> IO ()
setParameter au pid scp elt val offsetSampleFrame = do
  c_AudioUnitSetParameter au pid scp elt val (toEnum offsetSampleFrame)
  return ()
