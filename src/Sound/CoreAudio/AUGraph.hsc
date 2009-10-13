{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module Sound.CoreAudio.AUGraph (
  ComponentDescription (..),
  newAUGraph,
  addNode
) where

import System.Mac.ComponentDescription
import System.Mac.OSStatus
import Foreign
import Foreign.C.Types

data OpaqueAUGraph
newtype AUGraph = AUGraph (Ptr OpaqueAUGraph)
type AUNode = (AUGraph, CInt)

foreign import ccall "AUGraph.h NewAUGraph"
  c_NewAUGraph :: Ptr (Ptr OpaqueAUGraph) -> IO OSStatus

newAUGraph :: IO AUGraph
newAUGraph = alloca $ \ptr -> do
  requireNoErr $ c_NewAUGraph ptr
  aup <- peek ptr
  return $ AUGraph aup
{-# INLINE newAUGraph #-}

foreign import ccall "AUGraph.h AUGraphAddNode"
  c_AUGraphAddNode :: Ptr OpaqueAUGraph ->
                      Ptr ComponentDescription ->
                      Ptr CInt ->
                      IO OSStatus

addNode :: AUGraph -> ComponentDescription -> IO AUNode
addNode g@(AUGraph auP) cd = do
  alloca $ \nodeP -> do
  alloca $ \cdP -> do
    poke cdP cd
    requireNoErr $ c_AUGraphAddNode auP cdP nodeP
    ni <- peek nodeP
    return (g, ni)
{-# INLINE addNode #-}
