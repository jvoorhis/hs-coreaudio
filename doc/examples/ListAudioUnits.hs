{-# LANGUAGE OverloadedStrings #-}

module QueryAudioUnits where

import Control.Monad
import Prelude hiding (any)
import System.Mac.Components

-- Define ComponentDescriptions for AudioUnits in example
dlsSynth = ComponentDescription {
  componentType         = "aumu", -- OSTypes can be denoted as literal strings
  componentSubType      = "dls",  -- as long as OverloadedStrings are enabled
  componentManufacturer = "appl",
  componentFlags        = 0,
  componentFlagsMask    = 0
}

defOut = ComponentDescription {
  componentType         = "auou",
  componentSubType      = "def",
  componentManufacturer = "appl",
  componentFlags        = 0,
  componentFlagsMask    = 0
}

componentTypes = [("auou", "Output"),
                  ("aumu", "Music Devices"),
                  ("aumf", "Music Effects"),
                  ("aumc", "Format Converters"),
                  ("aufx", "Effects"),
                  ("aumx", "Mixers"),
                  ("aupn", "Panners"),
                  ("auol", "Offline Effects"),
                  ("augn", "Generators")]

main = forM_ componentTypes $ \(typ, desc) -> do
  putStrLn desc
  putStrLn $ replicate 30 '-'
  cs <- components $ ComponentDescription typ any any 0 0
  forM_ cs $ \c -> do
    name <- componentName c
    maybe (putStrLn "No name given") putStrLn name
    info <- componentInfo c
    maybe (putStrLn "No info given") putStrLn info
    putStrLn ""
