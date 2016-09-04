{-# LANGUAGE RecordWildCards, NamedFieldPuns, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-|

@
stack build && stack exec -- it portaudio
@

@
brew install portaudio

ls /usr/local/opt/portaudio/lib
@

more examples:

https://github.com/sw17ch/portaudio/tree/master/examples

The microphone data is formatted as PCM?

haskell FLAC?


flac:

http://superuser.com/questions/553503/what-is-the-difference-between-wav-and-flac

"""
FLAC is a lossless audio codec (its container also happens to be called FLAC, but the main idea here is the actual codec).
WAV, on the other hand, as a container can hold numerous kinds of audio codecs, but mostly, you'll find PCM-encoded audio.
So, simply put: Take a WAV file with PCM-encoded audio, and the corresponding (mathematically equal) FLAC file will be a tad smaller. The downside is that FLAC is not as widely supported as WAV. For example, most (all?) operating systems won't play or convert FLAC files without extra software.
"""

@
flac  --channels 1  --sample-rate 16000  --endian little  --sign signed  --bps 16  -o microphone.flac  --force-raw-format  microphone.wav
@

because:

* portaudio sets the number of channels and the sample rate.
* printing the buffer, I see negatives. samples are Int16.
* the architecture of my machine is Intel (About This Mac: 1.8 GHz Intel Core i5)
* bits per sample: samples are Int16.

@
flac  --channels 1  --sample-rate 16000  --endian little  --sign signed  --bps 8 -o final.flac  --force-raw-format  final.wav
@

@
$ du *
# the size of each file in the current directory, in bytes
@

-}
module Example.PortAudio where

import Sound.PortAudio
import Sound.PortAudio.Base
import qualified Data.ByteString.Lazy as BL

import Control.Monad (forever)
import Data.Int (Int8) -- (Int16): with Int16, readStream/writeStream always succeed
import Data.Word (Word8)
import Data.Monoid ((<>))
import Foreign (ForeignPtr,mallocForeignPtrArray,withForeignPtr,peekArray)
import Control.Arrow ((>>>))
import Control.Concurrent.STM
import Control.Concurrent (forkIO,threadDelay)
import System.IO (openBinaryFile,IOMode(..),hClose)
import Control.Exception (finally)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Foldable (Foldable(..))
import System.FilePath ((</>))

--------------------------------------------------------------------------------

finalAudioFile = audioDir </> "final.raw"
mkContinuousAudioFile i = audioDir </> ("continuous."++(show (i::Int))++".raw")
audioDir = "audio"

main :: IO ()
main = do
  vAudio <- newTVarIO Seq.empty

  saveOnUserInterrupt finalAudioFile vAudio $ do
    main' vAudio

main' vAudio = do
  stream <- initPortAudio sFramesPerBuffer
  putStrLn "(Listening...)"

  _ <- forkIO $ forever $ do
    let nChannels = 1 --TODO
    Right nSamples <- readAvailable stream
    let nSize = nChannels * nSamples
    buffer <- mallocForeignPtrArray nSize  --TODO resource-safety
    _ <- readStream  stream (fromIntegral nSamples) buffer
    _ <- writeStream stream (fromIntegral nSamples) buffer
    -- printBPSBuffer nSize buffer
    appendAudio vAudio nSize buffer
    return ()

  _ <- forkIO $ saveAudioContinuously vAudio 1

  busyWait

  where
  sFramesPerBuffer = 0x800

saveOnUserInterrupt file vAudio io = io `finally` saveAudio file vAudio

busyWait = forever $ wait 30

wait = threadDelay . (*1000)

saveAudioContinuously vAudio i = do
    -- removeFile (mkContinuousAudioFile i) -- lol why even param
    () <- saveAudio (mkContinuousAudioFile i) vAudio -- match makes blocking, cf lazy-IO? lol maybe.
    wait 30
    saveAudioContinuously vAudio (1+(i::Int))

--------------------------------------------------------------------------------

-- | bits-per-sample
type BPS = Int8 -- Int16 -- Word8

type PCM = Seq BPS

{-

:: [Int16] -> ByteString

:: Int16 -> (Word8,Word8)
but endianness? yuck.
to convert raw audio data to Flac, we have to be aware of the endianness anyways

Data.Bits
testBit :: a -> Int -> Bool

toIntegralSized :: (Integral a, Integral b, Bits a, Bits b) => a -> Maybe b
Attempt to convert an Integral type a to an Integral type b using the size of the types as measured by Bits methods.

https://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Bits.html

:: Int8 -> Word8


-}
saveAudio :: FilePath -> TVar PCM -> IO ()
saveAudio file vAudio = do
  audio_shorts <- atomically $ readTVar vAudio
  let audio_bytes = convert . toList $ audio_shorts

  h <- openBinaryFile file WriteMode
  () <- BL.hPut h audio_bytes
  hClose h

  where

  convert :: [Int8] -> BL.ByteString
  convert = fmap int2word  >>> BL.pack

  int2word :: Int8 -> Word8
  int2word = fromIntegral

  -- convert :: [Int16] -> BL.ByteString
  -- convert = fmap short2words >>> concatMap (\(x,y) -> [x,y]) >>> BL.pack

  -- -- little-endian
  -- short2words :: Int16 -> (Word8,Word8)
  -- short2words = undefined

{-|

-}
appendAudio :: TVar PCM -> Int -> ForeignPtr BPS -> IO ()
appendAudio var size _buffer = withForeignPtr _buffer $ \buffer -> do
  is <- peekArray size buffer
  atomically $ modifyTVar var (<> Seq.fromList is)
    -- Disk usage grows polynomially, when saved. Instead, don't depend, and just save chunks

{-|

e.g. one frame:

@
[1,13,26,31,5,-28,-23,-8,16,45,28,10,14,9,-11,-36,-23,-4,-3,0,-18,-20,-6,-17,-16,-15,-9,8,26,26,8,16,13,-10,-17,-32,-23,-10,-20,-24,-40,-40,-22,-3,11,-4,-42,-60,-27,3,9,-11,-23,-13,-32,-20,-16,-32,-36,-17,-7,-16,-36,-52,-33,-37,-36,-16,-1,8,0,-11,-23,-29,-33,-36,-7,27,33,16,8,10,-5,-8,13,23,21,30,38,34,28,4,-14,-6,-2,-4,7,-1,6,3,-30,-18,-22,-39,-6,15,3,9,15,4,0,-8,-3,20,25,8,1,14,14,23,41,31,12,19,15,39,53,53,58,39,28,30,54,64,44,49,63,52,52,61,37,34,30,14,34,17,-7,-1,-7,-11,0,4,-2,-6,-1,-6,-1,-4,-10,-13,-11,-7,-40,-42,-26,-44,-43,-43,-38,-27,-49,-52,-59,-84,-94,-76,-41,-19,1,0,-20,-26,-37,-31,-47,-46,-4,11,10,-6,-20,-38,-19,7,-11,-5,-18,-15,25,26,12,20,22,14,34,33,25,8,-29,-41,-25,-1,-2,19,51,55,56,41,14,12,32,53,50,64,56,31,33,32,26,16,17,26,39,34,29,20,22,46,57,56,63,29,0,5,-5,-9,-7,-3,1,24,28,28,42,27,29,60,68,77,68,29,4,-3,-2,16,33,14,-21,-16,1,7,16,-2,-33,-18,4,13,29,21,13,-10,-21,-28,-52,-62,-53,-44,-30,-6,15,25,1,-38,-32,-29,-32,-31,-6,-1,-16,-13,-27,-55,-80,-64,-52,-38,-20,-9,-9,-15,-23,-34,-42,-65,-74,-62,-55,-64,-81,-61,-43,-57,-58,-57,-48,-48,-48,-34,-41,-34,-12,-5,-19,-47,-53,-42,-30,-13,4,4,-5,-3,0,-3,-4,19,36,27,36,52,70,79,43,25,35,43,66,63,39,26,41,42,22,37,40,17,17,28,44,45,43,24,13,20,7,20,22,3,-3,-7,-5,5,10,21,39,46,51,59,49,33,7,10,17,-33,-9,14,5,42,59,26,-1,4,2,22,40,51,53,32,36,54,51,39,30,31,32,21,22,33,40,40,17,-1,0,-7,-5,-3,-10,4,4,3,17,21,22,9,-11,-28,-30,-31,-44,-23,9,-2,-5,3,-8,-22,-58,-54,-40,-24,-11,-18,-7,-16,-34,-55,-68,-70,-41,-10,0,16,-27,-52,-44,-50,-41,-41,-47,-58,-51,-34,-36,-54,-47,-22,-8,-11,-32,-60,-62,-44,-26,-13,-22,-34,-19,-18,-48,-60,-43,-3,0,16,44,47,32,-3,0,-7,-22,-35,-29,-13,-8,1,19,7,-35,-43,-30,-17,-6,-13,-17,-4,-18,-7,0,-9,-2,-15,-13,-5,11,33,24,15,15,28,31,29,19,5,5,-9,-16,-14,-14,-7,1,19,13,-11,-19,-22,-20,-29,-36,-31,-20,-5,2,0,-2,-11,-16,-3,3,12,21,16,31,65,66,43,39,33,31,35,37,28,31,65,79,71,50,36,50,77,89,82,82,88,89,82,60,46,17,-12,-12,-13,5,14,-3,-2,2,-2,9,37,22,0,-14,-23,-37,-45,-39,-60,-84,-92,-97,-77,-49,-58,-78,-89,-63,-53,-46,-44,-54,-44,-74,-76,-44,-73,-92,-90,-92,-88,-52,-30,-36,-29,-28,-7,-9,-42,-33,-34,-28,-16,-38,-37,-33,-43,-29,-32,-31,-2,3,13,33,37,38,31,6,-12,-7,5,7,10,30,49,47,36,25,12,16,8,5,12,25,30,29,41,57,50,14,22,26,10,40,72,91,94,71,58,41,36,39,37,27,27,49,61,52,60,59,67,79,56,59,75,89,99,79,62,51,41,32,26,5,11,34,31,28,24,37,43,27,42,47,18,-25,-20,1,-20,-26,2,40,59,66,58,27,31,46,41,38,33,37,72,80,47,4,-36,-7,0,-25,-9,7,17,-16,-26,-6,5,5,-22,-31,-46,-55,-54,-74,-92,-98,-92,-73,-58,-43,-46,-62,-74,-75,-75,-45,-28,-51,-48,-66,-52,-75,-117,-93,-120,-88,-39,-36,-19,-15,-44,-19,14,16,6,-75,-96,-34,-3,6,5,-23,-29,-14,-9,0,0,23,31,37,64,61,47,43,45,26,23,41,33,-12,-45,-52,-34,-34,-64,-42,-30,-7,66,71,8,-4,22,35,29,40,8,-11,30,52,40,33,27,-37,-56,-31,-7,4,7,34,54,65,50,47,23,-15,9,35,27,34,26,16,1,-12,-14,-13,9,22,16,2,12,-5,-19,-15,-6,-3,-2,19,9,-11,-31,-48,-39,-41,-49,-38,-13,-14,-17,-8,-3,-1,4,0,-37,-57,-52,-37,-22,-6,2,6,2,-23,-30,-21,-29,-19,-9,3,8,-8,-17,-22,-24,-35,-23,0,11,5,2,7,9,1,-22,-22,-21,-9,-5,-21,-10,8,25,62,61,48,0,-27,-8,-23,0,28,49,49,36,36,12,-7,7,22,17,29,33,27,41,44,35,18,25,34,33,55,56,31,25,29,15,29,26,1,11,31,43,25,16,16,6,-1,-24,-62,-77,-51,-24,-28,-35,-46,-43,-12,-19,-33,-45,-35,-35,-45,-43,-43,-19,-25,-25,-24,-27,-18,-19,-6,4,24,33,14,6,-1,-11,1,1,-8,-7,12,25,7,15,15,-1,3,21,16,11,25,31,35,20,-5,-15,2,24,29,46,48,25,5,20,32,44,30,16,28,4,8,0,-1,39,59,78,59,-2,-87,-147,-119,-31,39,59,40,-25,-31,8,-1,-15,-24,-39,-21,3,22,12,-43,-48,-5,-3,-1,-4,-11,-30,-20,-17,-66,-63,-32,-37,-36,-31,-35,-35,-47,-58,-67,-81,-72,-45,-29,0,18,-5,-29,-35,-27,-8,16,16,17,13,15,31,44,44,16,0,1,37,67,77,73,52,48,59,48,27,9,2,21,46,62,62,55,55,61,59,37,24,24,25,7,-8,-21,-34,-1,15,5,15,39,34,13,18,25,24,0,-1,8,15,22,13,29,25,-6,-4,-24,-38,-34,-39,-12,1,0,0,13,18,-9,-46,-30,-24,-40,-13,10,25,20,9,-8,-11,-17,-31,-28,-17,-19,-39,-36,-34,-28,-16,-27,-20,-33,-47,-31,-32,-22,-12,0,11,-3,-26,-29,-24,-17,-9,-20,-11,-21,-37,-46,-69,-52,-41,-31,-28,-25,11,6,-4,13,10,5,8,21,12,10,16,7,14,19,19,23,24,14,19,-15,-35,-15,-29,-43,-30,0,33,58,49,18,0,-24,-26,4,17,1,-1,3,2,-5,-1,13,8,-2,-7,-1,9,15,24,25,12,8,17,20,21,20,11,-9,-4,-7,-21,-26,-29,-17,-21,-13,0,22,39,17,-6,-13,-17,-16,-20,-8,-1,-12,-5,18,36,40,22,16,13,21,21,8,0,-5,0,-4,-4,-4,-7,-11,-14,1,15,16,32,13,-3,18,17,43,34,19,6,-27,8,32,19,-1,-7,-9,18,58,70,75,66,58,54,31,15,3,-1,1,-17,-8,7,-17,-28,0,24,12,-1,-5,-26,-30,3,29,29,-3,-32,-22,2,0,-22,-25,-25,-18,-6,-2,-5,-6,-24,-36,-9,-16,-25,-27,-48,-63,-51,-21,8,14,-13,-14,-17,-9,-19,-22,-16,-34,-24,-16,-7,-15,-46,-32,0,1,1,-13,-12,7,-7,2,24,20,28,19,0,-13,-40,-45,-17,-16,-45,-59,-26,-19,-45,-57,-46,-16,-16,-12,-19,-29,-16,-23,-16,-19,-28,-26,-12,-23,-24,1,-1,-12,-17,-18,4,2,1,-12,-25,17,24,16,45,54,41,28,36,35,38,38,34,54,50,37,45,48,24,23,26,-5,-5,14,35,26,1,4,6,-5,-4,4,1,0,13,22,17,-3,-30,-47,-49,-25,-18,-3,-10,-31,-21,-26,-25,-26,-40,-37,-48,-65,-34,-19,-17,-17,-32,-38,-29,-3,15,15,-6,-15,0,-9,-19,-13,-17,-10,0,-1,-3,-4,0,-1,1,20,29,27,8,0,14,34,51,43,31,29,20,19,38,52,31,23,11,-14,13,27,52,52,39,67,64,54,46,36,51,76,85,84,71,61,66,54,39,41,22,24,33,20,43,45,23,15,8,14,19,9,0,9,3,4,-4,-19,-1,-8,-3,-10,-14,-14,-57,-73,-44,-37,-34,-14,9,21,-3,-38,-34,-16,-4,-2,-32,-31,-5,4,12,0,7,27,22,50,62,40,14,-20,-34,-26,-26,-23,-8,7,35,55,64,30,0,7,-4,6,-9,-19,-19,-30,-26,-39,-56,-60,-38,-16,-18,-30,-33,-24,-50,-62,-41,-36,-28,-26,-16,-7,-3,-13,-44,-38,-32,-45,-35,-31,-5,18,7,12,-2,-23,-49,-65,-52,-50,-68,-52,-23,-34,-46,-51,-46,-20,-20,-33,-10,11,13,8,12,14,3,-5,-11,-18,-10,13,24,12,13,22,15,12,18,21,19,4,1,16,13,30,44,33,35,37,25,-13,-24,-12,-3,4,0,-14,-21,12,31,12,1,-11,-37,-36,-19,-12,5,21,16,-1,-22,-34,-31,-39,-37,-30,-20,-22,-13,-10,-11,10,2,4,8,0,19,48,44,44,43,35,37,30,23,17,28,15,4,33,34,22,34,30,15,7,8,25,12,-25,-9,27,39,48,38,24,16,11,25,37,12,16,32,42,46,29,19,10,-7,-35,-37,-12,6,-11,-27,-35,-8,-2,-30,-36,-42,-32,-3,18,3,-11,-16,-23,-33,-51,-44,-16,3,15,-4,-19,-26,-22,-21,-28,-13,-34,-27,-4,3,25,31,22,4,13,18,34,34,8,31,22,-1,5,10,11,35,21,-21,-9,-9,-15,-27,-25,-5,8,14,14,32,15,-16,-20,-9,-6,-29,-17,3,2,11,8,17,29,21,8,-10,-15,3,26,21,3,2,3,8,23,22,16,18,3,17,34,32,27,14,-7,-29,-18,11,27,6,-21,-18,-9,-11,-16,-23,-10,0,10,14,15,23,16,-1,-2,5,1,23,42,36,38,19,8,2,12,31,1,16,33,7,13,28,28,24,27,14,2,13,28,25,18,24,25,32,61,57,12,-25,-25,-21,-17,-9,-8,17,28,12,14,-6,-27,-28,-22,-13,-30,-30,-51,-50,-30,-23,0,-42,-38,-26,-48,-43,-53,-48,-41,-25,-31,-39,-45,-40,-22,-13,-6,2,22,10,-15,-23,-19,1,24,21,-4,2,-7,-36,-20,-18,-44,-43,-14,17,10,-6,-7,-17,-20,-19,-20,-11,-7,2,-3,-39,-45,-29,-34,-40,-29,-39,-28,-8,-14,13,24,3,-11,-14,-14,-20,-51,-73,-78,-79,-48,-29,-42,-54,-49,-30,-21,-46,-41,-35,-57,-68,-78,-46]
@

-}
printBPSBuffer :: Int -> ForeignPtr BPS -> IO ()
printBPSBuffer size _buffer = withForeignPtr _buffer $ \buffer -> do
  is <- peekArray size buffer
  case is of
    [] -> putStr "." -- skip silence
    _  -> print is

{-

readStream  :: Stream input output -> CULong -> ForeignPtr input  -> IO ()
writeStream :: Stream input output -> CULong -> ForeignPtr output -> IO ()

ForeignPtr a
its length must be the number of frames times the channels in the underlying stream

mallocForeignPtrArray :: Storable a => Int -> IO (ForeignPtr a)

"It uses pinned memory in the garbage collected heap, so the ForeignPtr does not require a finalizer to free the memory. Use of mallocForeignPtr and associated functions is strongly recommended in preference to newForeignPtr with a finalizer."

-}

initPortAudio :: Int -> IO (Stream BPS BPS)
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

  Right (stream :: Stream BPS BPS) <- openStream' _stream

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

--------------------------------------------------------------------------------
