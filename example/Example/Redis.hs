{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-|

install:

@
nix-env -i redis
@

run:

@
redis-server

stack build && stack exec -- it redis
@

http://hackage.haskell.org/package/hedis-0.9.1/docs/Database-Redis.html

-}
module Example.Redis where

import Database.Redis
import Control.Monad.IO.Class


main = do
 -- connects to localhost:6379
 conn <- connect defaultConnectInfo
 runRedis conn $ do
     set "hello" "hello"
     set "world" "world"
     hello <- get "hello"
     world <- get "world"
     liftIO $ print (hello,world)
