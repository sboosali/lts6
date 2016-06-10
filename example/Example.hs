{-# LANGUAGE LambdaCase #-}

module Example where
import qualified Example.WebSockets
import qualified Example.Wreq
import qualified Example.PortAudio

import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= \case
  ["websockets"] -> Example.WebSockets.main
  ["wreq"]       -> Example.Wreq.main
  ["portaudio"]  -> Example.PortAudio.main
  _              -> Example.PortAudio.main
