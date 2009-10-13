{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module Sound.CoreAudio.AUGraph (
  ComponentDescription (..),
  new,
  addNode
) where

import System.Mac.ComponentDescription
import System.Mac.OSStatus
import Foreign (Ptr, Storable (..), alloca)
import Foreign.C.Types (CInt)

data OpaqueAUGraph
newtype AUGraph = AUGraph (Ptr OpaqueAUGraph)
type AUNode = (AUGraph, CInt)

foreign import ccall "AUGraph.h NewAUGraph"
  c_NewAUGraph :: Ptr (Ptr OpaqueAUGraph) -> IO OSStatus

new :: IO AUGraph
new = alloca $ \ptr -> do
  requireNoErr $ c_NewAUGraph ptr
  aup <- peek ptr
  return $ AUGraph aup
{-# INLINE new #-}

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
