{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-type-defaults #-}
module Example.Wreq where

import Network.Wreq
import Control.Lens
--import Data.Aeson
--import Data.Aeson.Lens
import qualified Data.ByteString.Lazy.Char8 as B
import Data.ByteString.Lazy (ByteString)
--import qualified Data.Text.IO as T

import Data.Monoid ((<>))
import Data.Foldable (traverse_)
import System.Environment

default(Integer)

main = do
  apikey <- getEnv "apikey"                     -- secret
  print apikey
  r <- postGoogleSpeechAPI apikey "audio.json"
  traverse_ B.putStrLn $ r ^? responseBody

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

-}

--------------------------------------------------------------------------------

type APIKey = String


{-|

e.g.

@
do
  apikey <- getEnv "apikey"                     -- secret
  r <- postGoogleSpeechAPI apikey "audio.json"  -- json request, has base64-encoded audio
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
postGoogleSpeechAPI :: APIKey -> FilePath -> IO (Response ByteString)
postGoogleSpeechAPI apikey file = do
  audio <- B.readFile file
  r <- postWith o (url <> apikey) audio
  return r

  where
  o = defaults
      & header "Content-Type" .~ ["application/json"]

  url = "https://speech.googleapis.com/v1/speech:recognize?key="

