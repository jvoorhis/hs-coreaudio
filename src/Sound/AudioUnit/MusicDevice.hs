{-# LANGUAGE ForeignFunctionInterface, PatternGuards #-}

-- | Module containing types and operations corresponding to the C types
-- defined in @MusicDevice.h@ in the @AudioUnit.framework@.

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

-- | Calls @MusicDeviceMIDIEvent@ as defined in @MusicDevice.h@.
midiEvent :: MusicDeviceComponent
          -> Int -- ^ MIDI status byte
          -> Int -- ^ first data byte
          -> Int -- ^ second data byte, may be 0
          -> Int -- ^ The event's offset from the first sample frame.
                 --   You will typically pass 0.
          -> IO ()
midiEvent device status data1 data2 offsetSampleFrame
  | st <- toEnum status,   d1 <- toEnum data1
  , d2 <- toEnum data2,    sf <- toEnum offsetSampleFrame
  = requireNoErr $ c_MusicDeviceMIDIEvent device st d1 d2 sf
{-# INLINE midiEvent #-}
