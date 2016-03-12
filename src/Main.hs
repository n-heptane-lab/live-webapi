{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy as Lazy
import Control.Concurrent (threadDelay)
import Control.Exception (bracket, bracket_)
import Data.Aeson
import Data.Text
import System.MIDI as MIDI
import System.MIDI.Utility as MIDI
import System.ZMQ as ZMQ

data Val
    = VInteger Integer
    | VDouble Double
    | VText Text
    | VId Text
      deriving Show

class ToVal v where toVal :: v -> Val
instance ToVal Integer    where toVal = VInteger
instance ToVal Double where toVal = VDouble
instance ToVal Text   where toVal = VText

data Request
    = Get  Text
    | Set  Text Val
    | Call Text [Val]
      deriving Show

instance ToJSON Val where
    toJSON (VInteger v)    = toJSON v
    toJSON (VDouble v) = toJSON v
    toJSON (VText v)   = toJSON v
    toJSON (VId v)     = toJSON v

instance ToJSON Request where
    toJSON (Get path) =
        object [ "method" .= ("GET" :: Value)
               , "path"   .= toJSON path
               ]
    toJSON (Set path value) =
        object [ "method" .= ("GET" :: Value)
               , "path"   .= toJSON path
               , "value"  .= toJSON value
               ]
    toJSON (Call path parameters) =
        object [ "method"     .= ("CALL" :: Value)
               , "path"       .= toJSON path
               , "parameters" .= toJSON parameters
               ]

set :: (ToVal v) => Text -> v -> Request
set path v = Set path (toVal v)

get :: Text -> Request
get path = Get path
{-
data Response
    = Result { messageId :: Maybe Integer
             , 
-}
pullAddress = "tcp://127.0.0.1:5554"
pushAddress = "tcp://127.0.0.1:5553"
subAddress  = "tcp://127.0.0.1:5552"

-- | main
main :: IO ()
main =
    bracket (createSource "WebAPI") disposeConnection $ \midiOut ->
    bracket (createDestination "WebAPI" Nothing) disposeConnection $ \midiIn ->
    bracket_ (start midiOut) (MIDI.close midiOut) $
    bracket_ (start midiIn) (MIDI.close midiIn) $
    withContext 1 $ \context ->
    withSocket context Pull $ \pullSocket ->
    withSocket context Push $ \pushSocket ->
    withSocket context Sub  $ \subSocket -> do
        let sendRequest req = ZMQ.send pushSocket (Lazy.toStrict $ encode req) []
            recvResponse = (decodeStrict' <$> receive pullSocket []) :: IO (Maybe Value)
        -- subscription
        putStrLn "Connecting to subscription."
        connect subSocket subAddress
        putStrLn "Connected."
        subscribe subSocket ""
        -- pull
        putStrLn "Connecting to pull."
        connect pullSocket pullAddress
        putStrLn "Connected."
        -- push
        putStrLn "Connecting to push."
        connect pushSocket pushAddress
        putStrLn "Connected."
        threadDelay 2000000
        putStrLn "Signal wakeup."
        sendSysEx' midiOut []
        putStrLn "Signalled."
        -- push a request
        putStrLn "Pushing a request."
--        ZMQ.send pushSocket "{ method: set, path: is_playing, value: true }" []
--        ZMQ.send pushSocket "{ \"method\" : \"SET\", \"path\": \"live_set.is_playing\", \"value\" : true }" []
--        ZMQ.send pushSocket "{ \"method\" : \"CALL\", \"path\": \"document.create_midi_track\", \"parameters\" : [-1] }" []
--        ZMQ.send pushSocket "{ \"method\" : \"GET\", \"path\": \"document.tracks\" }" []
        sendRequest (Get "document.tracks")
        putStrLn "Pushed."
        -- signalling via sysex
        -- pull response
        putStrLn "Pulling a response."
--        r <- receive pullSocket []
        r <- recvResponse
        putStrLn $ "Pulled."
        print r
        -- loop
--        let go = do
--              sub <- receive subSocket []
--              print sub
--              go
--        go


