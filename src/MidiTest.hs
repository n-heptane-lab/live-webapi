{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket, bracket_)
import System.MIDI as MIDI
import System.MIDI.Utility as MIDI


-- | main
main :: IO ()
main =
    bracket (createSource "WebAPI") disposeConnection $ \midiOut ->
    bracket (createDestination "WebAPI" Nothing) disposeConnection $ \midiIn ->
    bracket_ (start midiOut) (MIDI.close midiOut) $
     let go = do
--           m <- MIDI.send midiOut (MidiMessage 1 (NoteOn 60 100))
           m <- MIDI.sendSysEx' midiOut []
           threadDelay 1000000
           go
     in go

