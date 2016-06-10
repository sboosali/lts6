{-# LANGUAGE RecordWildCards, NamedFieldPuns, ScopedTypeVariables #-}
{-|


@
brew install portaudio

ls /usr/local/opt/portaudio/lib
@

-}
module Example.PortAudio where

import Sound.PortAudio
import Sound.PortAudio.Base

import Control.Monad (forever)
import Data.Int (Int16)
-- import Foreign.Ptr (nullPtr)
import Foreign.ForeignPtr (mallocForeignPtrArray)


main :: IO ()
main = do
  stream <- initPortAudio sFramesPerBuffer
  forever $ do
    let nChannels = 1 --TODO
    Right nSamples <- readAvailable stream
    let nSize = nChannels * nSamples
    buffer <- mallocForeignPtrArray nSize
    Nothing <- readStream  stream (fromIntegral nSamples) buffer
    Nothing <- writeStream stream (fromIntegral nSamples) buffer
    return ()

  where
  sFramesPerBuffer = 0x800

{-old

reading from garbagre memory, lol:

    let nChannels = 1 --TODO
    Right nSamplesReadable <- readAvailable stream
    Right nSamplesWritable <- writeAvailable stream
    let nSizeReadable = nChannels * nSamplesReadable
    let nSizeWritable = nChannels * nSamplesWritable
    input  <- mallocForeignPtrArray nSizeReadable
    output <- mallocForeignPtrArray nSizeWritable
    Nothing <- readStream  stream (fromIntegral nSamplesReadable) input
    Nothing <- writeStream stream (fromIntegral nSamplesWritable) output
    return ()

-}

{-

readStream  :: Stream input output -> CULong -> ForeignPtr input  -> IO ()
writeStream :: Stream input output -> CULong -> ForeignPtr output -> IO ()

ForeignPtr a
its length must be the number of frames times the channels in the underlying stream

mallocForeignPtrArray :: Storable a => Int -> IO (ForeignPtr a)

-}

initPortAudio :: Int -> IO (Stream Int16 Int16)
initPortAudio sFramesPerBuffer = do
  Nothing <- initialize
  Right (microphone,_) <- getDefaultInputInfo
  Right (speaker,   _) <- getDefaultOutputInfo

  let sInput =
        let
           spDevice           = microphone
           spChannelCount     = 1
           spSuggestedLatency = PaTime 0.1 -- fromIntegral 0.1
        in Just StreamParameters{..}

  let sOutput =
        let
           spDevice           = speaker
           spChannelCount     = 1
           spSuggestedLatency = PaTime 0.1
        in Just StreamParameters{..}

  let sCallback = Nothing

  let _stream = defaultOpenStream
                 { sInput
                 , sOutput
                 , sSampleRate = 16000
                 , sFramesPerBuffer = Just sFramesPerBuffer
                 , sCallback
                 }

  Right (stream :: Stream Int16 Int16) <- openStream' _stream

  Nothing <- startStream stream
  -- let zeroBlock = replicate sFramesPerBuffer [0]
  --TODO Nothing <- writeStream stream zeroBlock sFramesPerBuffer
  return stream

{-|
@
= OpenStream{..}
  where
  'sInput'           = Nothing
  'sOutput'          = Nothing
  'sSampleRate'      = 44100
  'sFramesPerBuffer' = Nothing
  'sFlags'           = []
  'sCallback'        = Nothing
  'sFinalizer'       = Nothing
@
-}
defaultOpenStream :: OpenStream input ouptut
defaultOpenStream  = OpenStream{..}
  where
  sInput           = Nothing
  sOutput          = Nothing
  sSampleRate      = 44100
  sFramesPerBuffer = Nothing
  sFlags           = []
  sCallback        = Nothing
  sFinalizer       = Nothing

{-|

e.g.

@
defaultOpenStream  = OpenStream input ouptut
defaultOpenStream  = OpenStream{..}
  where
  sInput           = Nothing
  sOutput          = Nothing
  sSampleRate      = 16000
  sFramesPerBuffer = Nothing
  sFlags           = []
  sCallback        = Nothing
  sFinalizer       = Nothing
@

-}
data OpenStream input output = OpenStream
 { sInput :: Maybe (StreamParameters input)         -- ^ Stream Parameters for Input Device,
                                                    -- or Nothing if we don't need Audio input for this Stream

 , sOutput :: Maybe (StreamParameters output)       -- ^ Stream Parameters for Output Device,
                                                    -- or Nothing if we don't need Audio output for this Stream

 , sSampleRate :: Double                            -- ^ Sample Rate for Input and Output Devices,
                                                    -- if you need distinct Sample Rates for input and output, then you should create seperate Streams

 , sFramesPerBuffer :: Maybe Int                    -- ^ Requested Frames Per Buffer,
                                                    -- or Nothing if you'd prefer the underlying library to send you the amount it sees fit

 , sFlags :: [StreamOpenFlag]                       -- ^ Various Parameters Dictating the Operation of the Callback Function

 , sCallback :: Maybe (StreamCallback input output) -- ^ Callback,
                                                    -- or Nothing for a blocking read/write stream

 , sFinalizer :: Maybe FinCallback                  -- ^ Callback on Completion,
                                                    -- or Nothing if no final processing necessary
 }

openStream'
  :: (StreamFormat input, StreamFormat output)
  => OpenStream input output
  -> IO (Either Error (Stream input output))
openStream' OpenStream{..} = openStream sInput sOutput sSampleRate sFramesPerBuffer sFlags sCallback sFinalizer

-- readStream' = readStream
