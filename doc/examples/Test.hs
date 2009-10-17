{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (threadDelay)
import Sound.AudioToolbox.AUGraph

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

note device pitch duration = do
  midiEvent device 0x90 pitch 100 0 -- note-on
  threadDelay $ floor $ duration * 1000 * 1000
  midiEvent device 0x80 pitch 100 0 -- note-off

main = do
  -- Setup an AUGraph with a synthesizer node fed into the default output
  graph <- new
  dls   <- addNode graph dlsSynth
  out   <- addNode graph defOut
  connect graph dls 0 out 0
  
  -- Initialize the graph and print its state
  open graph       -- load AudioUnit components specified by the graph
  initialize graph -- initialize AudioUnits
  start graph      -- begin rendering
  caShow graph
  
  -- Play some notes
  synth <- musicDevice graph dls -- dereference DLS AudioUnit
  note synth 60 0.5
  note synth 64 0.5
  note synth 67 0.5
  note synth 72 3
  threadDelay $ 2 * 1000 * 1000 -- hold while final note is sustained
  
  -- Stop rendering and release graph
  stop graph
  dispose graph
