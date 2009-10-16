{-# LANGUAGE EmptyDataDecls,
             ForeignFunctionInterface,
             OverloadedStrings,
             PatternGuards #-}

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
  componentDescription,
  audioUnit,
  musicDevice,
  musicDeviceMIDIEvent,
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
  peek ptr
{-# INLINE new #-}

foreign import ccall "AUGraph.h DisposeAUGraph"
  c_DisposeAUGraph :: AUGraph -> IO OSStatus

dispose :: AUGraph -> IO ()
dispose graph = requireNoErr $ c_DisposeAUGraph graph
{-# INLINE dispose #-}

foreign import ccall "AUGraph.h AUGraphAddNode"
  c_AUGraphAddNode :: AUGraph ->
                      Ptr ComponentDescription ->
                      Ptr AUNode ->
                      IO OSStatus

addNode :: AUGraph -> ComponentDescription -> IO AUNode
addNode graph cd = alloca $ \nodeP -> do
  alloca $ \cdP -> do
    poke cdP cd
    requireNoErr $ c_AUGraphAddNode graph cdP nodeP
    peek nodeP
{-# INLINE addNode #-}

foreign import ccall "AUGraph.h AUGraphConnectNodeInput"
  c_AUGraphConnectNodeInput :: AUGraph ->
                               AUNode -> CUInt ->
                               AUNode -> CUInt ->
                               IO OSStatus

connect :: AUGraph -> AUNode -> Int -> AUNode -> Int -> IO ()
connect graph n1 busOut n2 busIn =
  requireNoErr $ c_AUGraphConnectNodeInput graph
                   n1 (toEnum busOut)
                   n2 (toEnum busIn)
{-# INLINE connect #-}

foreign import ccall "AUGraph.h AUGraphOpen"
  c_AUGraphOpen :: AUGraph -> IO OSStatus

open :: AUGraph -> IO ()
open graph = requireNoErr $ c_AUGraphOpen graph
{-# INLINE open #-}

foreign import ccall "AUGraph.h AUGraphInitialize"
  c_AUGraphInitialize :: AUGraph -> IO OSStatus

initialize :: AUGraph -> IO ()
initialize graph = requireNoErr $ c_AUGraphInitialize graph
{-# INLINE initialize #-}

foreign import ccall "AUGraph.h AUGraphStart"
  c_AUGraphStart :: AUGraph -> IO OSStatus

start :: AUGraph -> IO ()
start graph = requireNoErr $ c_AUGraphStart graph
{-# INLINE start #-}

foreign import ccall "AUGraph.h AUGraphStop"
  c_AUGraphStop :: AUGraph -> IO OSStatus

stop :: AUGraph -> IO ()
stop graph = requireNoErr $ c_AUGraphStop graph
{-# INLINE stop #-}

foreign import ccall "AUGraph.h AUGraphNodeInfo"
  c_AUGraphNodeInfo :: AUGraph -> AUNode ->
                       Ptr ComponentDescription ->
                       Ptr AudioUnit ->
                       IO OSStatus

audioUnit :: AUGraph -> AUNode -> IO AudioUnit
audioUnit graph node = alloca $ \ptr -> do
  requireNoErr $ c_AUGraphNodeInfo graph node nullPtr ptr
  peek ptr
{-# INLINE audioUnit #-}

componentDescription :: AUGraph -> AUNode -> IO ComponentDescription
componentDescription graph node = alloca $ \ptr -> do
  requireNoErr $ c_AUGraphNodeInfo graph node ptr nullPtr
  peek ptr
{-# INLINE componentDescription #-}

newtype MusicDeviceComponent = MusicDeviceComponent ComponentInstance

musicDevice :: AUGraph -> AUNode -> IO MusicDeviceComponent
musicDevice graph node = do
  cd <- componentDescription graph node
  case componentType cd of
    "aumu" -> return . MusicDeviceComponent =<< audioUnit graph node
    _      -> error $ show (componentType cd) ++ " is not a MusicDevice"
{-# INLINE musicDevice #-}

foreign import ccall "MusicDevice.h MusicDeviceMIDIEvent"
  c_MusicDeviceMIDIEvent :: MusicDeviceComponent ->
                            CUInt -> CUInt -> CUInt -> CUInt ->
                            IO ComponentResult

musicDeviceMIDIEvent :: MusicDeviceComponent -> Int -> Int -> Int -> Int -> IO ()
musicDeviceMIDIEvent device status data1 data2 offsetSampleFrame
  | st <- toEnum status,   d1 <- toEnum data1
  , d2 <- toEnum data2,    sf <- toEnum offsetSampleFrame
  = requireNoErr $ c_MusicDeviceMIDIEvent device st d1 d2 sf
{-# INLINE musicDeviceMIDIEvent #-}

foreign import ccall "AUGraph.h CAShow"
  c_caShow :: AUGraph -> IO ()

caShow :: AUGraph -> IO ()
caShow graph = c_caShow graph
