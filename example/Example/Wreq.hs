{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-type-defaults #-}
module Example.Wreq where

import Network.Wreq
import Control.Lens
-- import Data.Aeson (toJSON)
import Data.Aeson.Lens
-- import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text.IO as T

import Data.Monoid ((<>))
import Data.Foldable (traverse_)
import System.Environment

default(Integer)

{-|

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

-}
main = do
  apikey <- getEnv "apikey"
  print apikey

  r <- postWith o (url <> "") (partFile "_" "test.json")
  traverse_ T.putStrLn $ r ^? (responseBody . key "files" . key "_" . _String)

o = defaults
 & header "Content-Type" .~ ["application/json"]

url = "http://httpbin.org/post"


