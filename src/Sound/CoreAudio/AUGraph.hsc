{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module Sound.CoreAudio.AUGraph (
  ComponentDescription (..),
  new,
  dispose,
  addNode,
  connect,
  open,
  initialize,
  start,
  stop,
  caShow
) where

import System.Mac.ComponentDescription
import System.Mac.OSStatus
import Foreign (Ptr, Storable (..), alloca)
import Foreign.C.Types -- (CInt, CUInt)

data OpaqueAUGraph
newtype AUGraph = AUGraph (Ptr OpaqueAUGraph)
type AUNode = CInt

foreign import ccall "AUGraph.h NewAUGraph"
  c_NewAUGraph :: Ptr (Ptr OpaqueAUGraph) -> IO OSStatus

new :: IO AUGraph
new = alloca $ \ptr -> do
  requireNoErr $ c_NewAUGraph ptr
  aup <- peek ptr
  return $ AUGraph aup
{-# INLINE new #-}

foreign import ccall "AUGraph.h DisposeAUGraph"
  c_DisposeAUGraph :: Ptr OpaqueAUGraph -> IO OSStatus

dispose :: AUGraph -> IO ()
dispose (AUGraph auP) = do
  requireNoErr $ c_DisposeAUGraph auP
  return ()
{-# INLINE dispose #-}

foreign import ccall "AUGraph.h AUGraphAddNode"
  c_AUGraphAddNode :: Ptr OpaqueAUGraph ->
                      Ptr ComponentDescription ->
                      Ptr CInt ->
                      IO OSStatus

addNode :: AUGraph -> ComponentDescription -> IO AUNode
addNode (AUGraph auP) cd = do
  alloca $ \nodeP -> do
  alloca $ \cdP -> do
    poke cdP cd
    requireNoErr $ c_AUGraphAddNode auP cdP nodeP
    node <- peek nodeP
    return node
{-# INLINE addNode #-}

foreign import ccall "AUGraph.h AUGraphConnectNodeInput"
  c_AUGraphConnectNodeInput :: Ptr OpaqueAUGraph ->
                               CInt -> CUInt ->
                               CInt -> CUInt ->
                               IO OSStatus

connect :: AUGraph -> (AUNode, Int) -> (AUNode, Int) -> IO ()
connect (AUGraph auP) (n1, busOut) (n2, busIn) = do
  requireNoErr $ c_AUGraphConnectNodeInput auP
                   n1 (toEnum busOut)
                   n2 (toEnum busIn)
  return ()
{-# INLINE connect #-}

foreign import ccall "AUGraph.h AUGraphOpen"
  c_AUGraphOpen :: Ptr OpaqueAUGraph -> IO OSStatus

open :: AUGraph -> IO ()
open (AUGraph auP) = do
  requireNoErr $ c_AUGraphOpen auP
  return ()
{-# INLINE open #-}

foreign import ccall "AUGraph.h AUGraphInitialize"
  c_AUGraphInitialize :: Ptr OpaqueAUGraph -> IO OSStatus

initialize :: AUGraph -> IO ()
initialize (AUGraph auP) = do
  requireNoErr $ c_AUGraphInitialize auP
  return ()
{-# INLINE initialize #-}

foreign import ccall "AUGraph.h AUGraphStart"
  c_AUGraphStart :: Ptr OpaqueAUGraph -> IO OSStatus

start :: AUGraph -> IO ()
start (AUGraph p) = do
  requireNoErr $ c_AUGraphStart p
  return ()
{-# INLINE start #-}

foreign import ccall "AUGraph.h AUGraphStop"
  c_AUGraphStop :: Ptr OpaqueAUGraph -> IO OSStatus

stop :: AUGraph -> IO ()
stop (AUGraph p) = do
  requireNoErr $ c_AUGraphStop p
  return ()
{-# INLINE stop #-}

foreign import ccall "AUGraph.h CAShow"
  c_caShow :: Ptr OpaqueAUGraph -> IO ()

caShow :: AUGraph -> IO ()
caShow (AUGraph p) = c_caShow p
