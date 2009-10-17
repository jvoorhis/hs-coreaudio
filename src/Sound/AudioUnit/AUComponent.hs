-- | Module containing types corresponding to the C types defined in
-- @AUComponent.h@ in the @AudioUnit.framework@.

module Sound.AudioUnit.AUComponent where

import System.Mac.Components (ComponentInstance)

type AudioUnit = ComponentInstance
