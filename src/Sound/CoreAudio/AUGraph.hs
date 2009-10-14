{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module Sound.CoreAudio.AUGraph (
  ComponentDescription (..),
  AUGraph,
  AUNode,
  AudioUnit,
  new,
  dispose,
  addNode,
  connect,
  open,
  initialize,
  start,
  stop,
  audioUnit,
  caShow
) where

import System.Mac.Components
import System.Mac.OSStatus
import Foreign (Ptr, Storable (..), alloca, nullPtr)
import Foreign.C.Types (CInt, CUInt)

data OpaqueAUGraph
type AUGraph = Ptr OpaqueAUGraph
type AUNode = CInt
type AudioUnit = ComponentInstance

foreign import ccall "AUGraph.h NewAUGraph"
  c_NewAUGraph :: Ptr AUGraph -> IO OSStatus

new :: IO AUGraph
new = alloca $ \ptr -> do
  requireNoErr $ c_NewAUGraph ptr
  graph <- peek ptr
  return graph
{-# INLINE new #-}

foreign import ccall "AUGraph.h DisposeAUGraph"
  c_DisposeAUGraph :: AUGraph -> IO OSStatus

dispose :: AUGraph -> IO ()
dispose graph = do
  requireNoErr $ c_DisposeAUGraph graph
  return ()
{-# INLINE dispose #-}

foreign import ccall "AUGraph.h AUGraphAddNode"
  c_AUGraphAddNode :: AUGraph ->
                      Ptr ComponentDescription ->
                      Ptr AUNode ->
                      IO OSStatus

addNode :: AUGraph -> ComponentDescription -> IO AUNode
addNode graph cd = do
  alloca $ \nodeP -> do
  alloca $ \cdP -> do
    poke cdP cd
    requireNoErr $ c_AUGraphAddNode graph cdP nodeP
    node <- peek nodeP
    return node
{-# INLINE addNode #-}

foreign import ccall "AUGraph.h AUGraphConnectNodeInput"
  c_AUGraphConnectNodeInput :: AUGraph ->
                               AUNode -> CUInt ->
                               AUNode -> CUInt ->
                               IO OSStatus

connect :: AUGraph -> (AUNode, Int) -> (AUNode, Int) -> IO ()
connect graph (n1, busOut) (n2, busIn) = do
  requireNoErr $ c_AUGraphConnectNodeInput graph
                   n1 (toEnum busOut)
                   n2 (toEnum busIn)
  return ()
{-# INLINE connect #-}

foreign import ccall "AUGraph.h AUGraphOpen"
  c_AUGraphOpen :: AUGraph -> IO OSStatus

open :: AUGraph -> IO ()
open graph = do
  requireNoErr $ c_AUGraphOpen graph
  return ()
{-# INLINE open #-}

foreign import ccall "AUGraph.h AUGraphInitialize"
  c_AUGraphInitialize :: AUGraph -> IO OSStatus

initialize :: AUGraph -> IO ()
initialize graph = do
  requireNoErr $ c_AUGraphInitialize graph
  return ()
{-# INLINE initialize #-}

foreign import ccall "AUGraph.h AUGraphStart"
  c_AUGraphStart :: AUGraph -> IO OSStatus

start :: AUGraph -> IO ()
start graph = do
  requireNoErr $ c_AUGraphStart graph
  return ()
{-# INLINE start #-}

foreign import ccall "AUGraph.h AUGraphStop"
  c_AUGraphStop :: AUGraph -> IO OSStatus

stop :: AUGraph -> IO ()
stop graph = do
  requireNoErr $ c_AUGraphStop graph
  return ()
{-# INLINE stop #-}

foreign import ccall "AUGraph.h AUGraphGetNodeInfo"
  c_AUGraphNodeInfo :: AUGraph ->
                       AUNode ->
                       Ptr ComponentDescription ->
                       Ptr AudioUnit ->
                       IO OSStatus

audioUnit :: AUGraph -> AUNode -> IO AudioUnit
audioUnit graph node = do
  alloca $ \ptr -> do
    requireNoErr $ c_AUGraphNodeInfo graph node nullPtr ptr
    peek ptr
{-# INLINE audioUnit #-}

foreign import ccall "AUGraph.h CAShow"
  c_caShow :: AUGraph -> IO ()

caShow :: AUGraph -> IO ()
caShow graph = c_caShow graph
