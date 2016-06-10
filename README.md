# Haskell experiments

Proof-of-concepts (to be moved somewhere useful).

## Running

    stack build
    stack exec it -- websockets
    stack exec it -- wreq
    stack exec it -- portaudio

## Example.WebSockets

connect a haskell websocket server to a python websocket client. via `websockets`.

## Example.Wreq

call `Google Cloud Speech API`. with `wreq`.

## Example.PortAudio

Echo the microphone to the speakers (Keep your volume low, or it grows exponentially to a screech). via `portaudio`.

works for me on `OSX Yosemite` with:

### Example.PortAudio Installation

    brew install portaudio

## Example._
