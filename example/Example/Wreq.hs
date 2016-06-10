{-# LANGUAGE OverloadedStrings, OverloadedLists, ExtendedDefaultRules, NoMonomorphismRestriction, RecordWildCards, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Example.Wreq where

import Network.Wreq
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Text (Text)

import Data.Monoid ((<>))
import Data.Foldable (traverse_)
import System.Environment (getEnv)
import System.IO (openBinaryFile,IOMode(..))

default(Integer)

main = do
  apikey <- getEnv "apikey"                     -- secret
  print apikey

  -- rJSON <- postGoogleSpeechJSON apikey "audio.json"
  -- traverse_ B.putStrLn $ rJSON ^? responseBody

  -- rBase64 <- postGoogleSpeechBase64 apikey "audio.base64"
  -- traverse_ BL8.putStrLn $ rBase64 ^? responseBody

  -- rFLAC <- postGoogleSpeechFLAC apikey "audio.flac"
  -- traverse_ BL8.putStrLn $ rFLAC ^? responseBody

  ts <- transcribeFLACGoogleSpeech apikey "audio.flac"
  traverse_ T.putStrLn ts

{-

1.

reverse curl with:

@
nc -l 6666 &
curl --header "Content-Type: application/json" --data-binary @request.json http://localhost:6666
@

showing:

@
POST / HTTP/1.1
Host: localhost:6666
User-Agent: curl/7.43.0
Accept: */*
Content-Type: application/json
Content-Length: 44739
Expect: 100-continue

{ ... }
@


2. err

StatusCodeException (Status {statusCode = 400, statusMessage = "Bad Request"}) [("Vary","Origin"),("Vary","X-Origin"),("Vary","Referer"),("Content-Type","application/json; charset=UTF-8"),("Content-Encoding","gzip"),("Date","Fri, 10 Jun 2016 16:15:32 GMT"),("Server","ESF"),("Cache-Control","private"),("X-XSS-Protection","1; mode=block"),("X-Frame-Options","SAMEORIGIN"),("X-Content-Type-Options","nosniff"),("Alternate-Protocol","443:quic"),("Alt-Svc","quic=\":443\"; ma=2592000; v=\"34,33,32,31,30,29,28,27,26,25\""),("Transfer-Encoding","chunked"),("X-Response-Body-Start","{\n  \"error\": {\n    \"code\": 400,\n    \"message\": \"Invalid JSON payload received. Unable to parse number.\\n------WebKitFormBoun\\n^\",\n    \"status\": \"INVALID_ARGUMENT\"\n  }\n}\n"),("X-Request-URL","POST https://speech.googleapis.com:443/v1/speech:recognize?key=AIzaSyC7zSWbJ8GJ9Ho3FZdfBVEvf35FrTOgTRA")] (CJ {expose = []})

( "X-Response-Body-Start"
, {
  "error": {
    "code": 400,
    "message": "Invalid JSON payload received. Unable to parse number.\n------WebKitFormBoun\n^",
    "status": "INVALID_ARGUMENT"
  }
}
)

The slightly more modern way to upload POST data is via a multipart/form-data payload, for which wreq provides the Part type.

ghci> r <- post "http://httpbin.org/post" [partText "button" "o hai"]
ghci> r ^. responseBody . key "headers" . key "Content-Type" . _String
"multipart/form-data; boundary=----WebKitFormBoundaryJsEZfuj89uj"

3.

"Base64 is a group of similar binary-to-text encoding schemes that represent binary data in an ASCII string format by translating it into a radix-64 representation."

"When you have some binary data that you want to ship across a network, you generally don't do it by just streaming the bits and bytes over the wire in a raw format. Why? because some media are made for streaming text. You never know -- some protocols may interpret your binary data as control characters (like a modem), or your binary data could be screwed up because the underlying protocol might think that you've entered a special character combination (like how FTP translates line endings).

So to get around this, people encode the binary data into characters. Base64 is one of these types of encodings. Why 64? Because you can generally rely on the same 64 characters being present in many character sets, and you can be reasonably confident that your data's going to end up on the other side of the wire uncorrupted."

-}

--------------------------------------------------------------------------------

type APIKey = String

data GoogleSpeechRequest = GoogleSpeechRequest
 { gAudio :: Text -- ByteString
 }

(-:) = (,)


{-|

e.g.

@
{
  "responses": [
    {
      "results": [
        {
          "alternatives": [
            {
              "transcript": "how old is the Brooklyn Bridge",
              "confidence": 0.98267895
            }
          ],
          "isFinal": true
        }
      ]
    }
  ]
}
@

-}
transcribeFLACGoogleSpeech :: APIKey -> FilePath -> IO [Text]
transcribeFLACGoogleSpeech apikey file = do
  r <- postGoogleSpeechFLAC apikey file
  let ts = r ^.. responseBody . _JSON . transcripts
  return ts

transcripts :: Traversal' Value Text
transcripts
  = key"responses"
  . _Array . each
  . key"results" --TODO filter by key"isFinal"
  . _Array . each
  . key"alternatives"
  . _Array . each
  . key"transcript"
  . _String


{-|

e.g.

@
do
  apikey <- getEnv "apikey"                     -- secret
  r <- postGoogleSpeech apikey "audio.json"  -- json request, has base64-encoded audio
@

port:

@
curl
 --header "Content-Type: application/json"
 --data-binary @request.json
 "https://speech.googleapis.com/v1/speech:recognize?key=$apikey"
@

where:

@
# request.json
{
  "initialRequest": {
    "encoding":"FLAC",
    "sampleRate":16000
  },
  "audioRequest": {
    "content": "$(cat audio.base64)"
  }
}
@

and

@
# audio.base64
...
@

-}
postGoogleSpeechJSON :: APIKey -> FilePath -> IO (Response ByteString)
postGoogleSpeechJSON apikey file = do
  audio <- BL8.readFile file
  r <- postWith optionsGoogleSpeech (urlGoogleSpeech <> apikey) audio
  return r

{-

e.g. reads @audio.base64@

-}
postGoogleSpeechBase64 :: APIKey -> FilePath -> IO (Response ByteString)
postGoogleSpeechBase64 apikey file = do
  h <- openBinaryFile file ReadMode
  _audio <- BS.hGetLine h                      -- base64 file has a final newline we must strip
  let gAudio = T.decodeUtf8 _audio
  let _json = request GoogleSpeechRequest{..}
  let _body = encode _json
  -- BL8.putStrLn _body
  r <- postWith optionsGoogleSpeech (urlGoogleSpeech <> apikey) _body
  return r

  -- json stores text, even though we're reading bytestring and decoding back into bytestring

{-

e.g. reads @audio.flac@

-}
postGoogleSpeechFLAC :: APIKey -> FilePath -> IO (Response ByteString)
postGoogleSpeechFLAC apikey file = do
  h <- openBinaryFile file ReadMode
  _flac <- BS.hGetContents h                      -- flac file has no final newline
  let _base64 = B64.encode _flac
  -- BS8.putStrLn _base64
  let gAudio = T.decodeUtf8 _base64
  let _json = request GoogleSpeechRequest{..}
  let _body = encode _json
  -- BL8.putStrLn _body
  r <- postWith optionsGoogleSpeech (urlGoogleSpeech <> apikey) _body
  return r

optionsGoogleSpeech = defaults
  & header "Content-Type" .~ ["application/json"]

urlGoogleSpeech = "https://speech.googleapis.com/v1/speech:recognize?key="

{-|

e.g.

@
{
  "initialRequest": {
    "encoding":"FLAC",
    "sampleRate":16000
  },
  "audioRequest": {
    "content": "$(cat audio.base64)"
  }
}
@

-}
request :: GoogleSpeechRequest -> Value
request GoogleSpeechRequest{..} = Object
  [ "initialRequest"-: Object
    [ "encoding"  -: String "FLAC"
    , "sampleRate"-: Number 16000
    ]
  , "audioRequest"-: Object
    [ "content"-: String gAudio
    ]
  ]

