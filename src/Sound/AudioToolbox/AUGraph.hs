{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface, OverloadedStrings, PatternGuards #-}

-- | Types and operations corresponding to the definitions in
-- @AudioToolbox/AUGraph.h@. For an overview of the api, see the section
-- titled "Hosting Audio Units" in the document at
-- <http://developer.apple.com/mac/library/documentation/MusicAudio/Conceptual/CoreAudioOverview/ARoadmaptoCommonTasks/ARoadmaptoCommonTasks.html>.

module Sound.AudioToolbox.AUGraph (
  ComponentDescription (..), -- | See "System.Mac.Components"
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
  midiEvent,
  caShow
) where

import Control.Applicative
import Foreign (Ptr, Storable (..), alloca, nullPtr)
import Foreign.C.Types (CInt, CUInt)
import System.Mac.Components hiding (componentDescription)
import System.Mac.OSStatus
import Sound.AudioUnit.AUComponent
import Sound.AudioUnit.MusicDevice

data OpaqueAUGraph

type AUGraph = Ptr OpaqueAUGraph

type AUNode = CInt

foreign import ccall "AUGraph.h NewAUGraph"
  c_NewAUGraph :: Ptr AUGraph -> IO OSStatus

-- | Creates a new 'AUGraph'
new :: IO AUGraph
new = alloca $ \ptr -> do
  requireNoErr $ c_NewAUGraph ptr
  peek ptr
{-# INLINE new #-}

foreign import ccall "AUGraph.h DisposeAUGraph"
  c_DisposeAUGraph :: AUGraph -> IO OSStatus

-- | Disposes an 'AUGraph'
dispose :: AUGraph -> IO ()
dispose graph = requireNoErr $ c_DisposeAUGraph graph
{-# INLINE dispose #-}

foreign import ccall "AUGraph.h AUGraphAddNode"
  c_AUGraphAddNode :: AUGraph
                   -> Ptr ComponentDescription
                   -> Ptr AUNode
                   -> IO OSStatus

-- | Add an 'AUNode' to the given 'AUGraph' specified by its
-- 'ComponentDescription'
addNode :: AUGraph -> ComponentDescription -> IO AUNode
addNode graph cd = alloca $ \nodeP -> do
  alloca $ \cdP -> do
    poke cdP cd
    requireNoErr $ c_AUGraphAddNode graph cdP nodeP
    peek nodeP
{-# INLINE addNode #-}

foreign import ccall "AUGraph.h AUGraphConnectNodeInput"
  c_AUGraphConnectNodeInput :: AUGraph
                            -> AUNode -> CUInt
                            -> AUNode -> CUInt
                            -> IO OSStatus

-- | Connect two 'AUNode's within an 'AUGraph'
connect :: AUGraph
        -> AUNode -- ^ source node
        -> Int    -- ^ source node's output bus, indexed by 0
        -> AUNode -- ^ target node
        -> Int    -- ^ target node's input bus, indexed by 0
        -> IO ()
connect graph n1 busOut n2 busIn =
  requireNoErr $ c_AUGraphConnectNodeInput graph
                   n1 (toEnum busOut)
                   n2 (toEnum busIn)
{-# INLINE connect #-}

foreign import ccall "AUGraph.h AUGraphOpen"
  c_AUGraphOpen :: AUGraph -> IO OSStatus

-- | Open the 'AudioUnit' components specified by an 'AUGraph'
open :: AUGraph -> IO ()
open graph = requireNoErr $ c_AUGraphOpen graph
{-# INLINE open #-}

foreign import ccall "AUGraph.h AUGraphInitialize"
  c_AUGraphInitialize :: AUGraph -> IO OSStatus

-- | Initialize the 'AudioUnit' components contained in an 'AUGraph'.
-- Should be invoked after a call to 'open'.
initialize :: AUGraph -> IO ()
initialize graph = requireNoErr $ c_AUGraphInitialize graph
{-# INLINE initialize #-}

foreign import ccall "AUGraph.h AUGraphStart"
  c_AUGraphStart :: AUGraph -> IO OSStatus

-- | Start rendering
start :: AUGraph -> IO ()
start graph = requireNoErr $ c_AUGraphStart graph
{-# INLINE start #-}

foreign import ccall "AUGraph.h AUGraphStop"
  c_AUGraphStop :: AUGraph -> IO OSStatus

-- | Stop rendering
stop :: AUGraph -> IO ()
stop graph = requireNoErr $ c_AUGraphStop graph
{-# INLINE stop #-}

foreign import ccall "AUGraph.h AUGraphNodeInfo"
  c_AUGraphNodeInfo :: AUGraph
                    -> AUNode
                    -> Ptr ComponentDescription
                    -> Ptr AudioUnit
                    -> IO OSStatus

-- | Dereference an 'AudioUnit' from an 'AUNode' and its 'AUGraph'
audioUnit :: AUGraph -> AUNode -> IO AudioUnit
audioUnit graph node = alloca $ \ptr -> do
  requireNoErr $ c_AUGraphNodeInfo graph node nullPtr ptr
  peek ptr
{-# INLINE audioUnit #-}

-- | Dereference a 'ComponentDescription' from an 'AUNode' and its 'AUGraph'
componentDescription :: AUGraph -> AUNode -> IO ComponentDescription
componentDescription graph node = alloca $ \ptr -> do
  requireNoErr $ c_AUGraphNodeInfo graph node ptr nullPtr
  peek ptr
{-# INLINE componentDescription #-}

-- | Obtain a 'MusicDeviceComponent' from an 'AUNode', if its 'componentType'
-- is @"aumu"@. Otherwise throws an exception.
musicDevice :: AUGraph -> AUNode -> IO MusicDeviceComponent
musicDevice graph node = do
  cd <- componentDescription graph node
  case componentType cd of
    "aumu" -> MusicDeviceComponent <$> audioUnit graph node
    _      -> error $ show (componentType cd) ++ " is not a MusicDevice"
{-# INLINE musicDevice #-}

foreign import ccall "AUGraph.h CAShow"
  c_caShow :: AUGraph -> IO ()

-- | Print a representation of an 'AUGraph' to @stdout@
caShow :: AUGraph -> IO ()
caShow graph = c_caShow graph
