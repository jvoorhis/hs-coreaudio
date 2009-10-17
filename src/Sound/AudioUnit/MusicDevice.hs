{-# LANGUAGE ForeignFunctionInterface, PatternGuards #-}

module Sound.AudioUnit.MusicDevice (
  MusicDeviceComponent (..),
  midiEvent
) where

import Foreign.C.Types (CUInt)
import System.Mac.Components
import System.Mac.OSStatus

newtype MusicDeviceComponent = MusicDeviceComponent ComponentInstance

foreign import ccall "MusicDevice.h MusicDeviceMIDIEvent"
  c_MusicDeviceMIDIEvent :: MusicDeviceComponent ->
                            CUInt -> CUInt -> CUInt -> CUInt ->
                            IO ComponentResult

midiEvent :: MusicDeviceComponent -> Int -> Int -> Int -> Int -> IO ()
midiEvent device status data1 data2 offsetSampleFrame
  | st <- toEnum status,   d1 <- toEnum data1
  , d2 <- toEnum data2,    sf <- toEnum offsetSampleFrame
  = requireNoErr $ c_MusicDeviceMIDIEvent device st d1 d2 sf
{-# INLINE midiEvent #-}
