{-# LANGUAGE OverloadedStrings #-}
module Example where
import qualified Data.Text          as T
import qualified Network.WebSockets as WS
-- import qualified Network.Wai.Handler.WebSockets as WAI

import           Control.Monad      (forever)
import           Data.Monoid      ((<>))

{-|

to start server:

@
stack build && stack exec it
@

clients send data to:

@
127.0.0.1:8888
@

https://ocharles.org.uk/blog/posts/2013-12-19-websockets.html

-}
main :: IO ()
main = do
  putStrLn "[listening...]"
  WS.runServer "127.0.0.1" 8888 (application 0)

-- handleConnection will be invoked every time a client connects to the server.
-- we need to accept the connection to complete the handshake.
application initial pending = do
  connection <- WS.acceptRequest pending
  respond connection (initial ::Int)


respond connection i = do                 -- e.g. 1
    request <- WS.receiveData connection  -- "2" (await)
    let j = read (T.unpack request)       -- 2
    let k = i+j                           -- 3
    let response = T.pack (show k)        -- "3"
    WS.sendTextData connection response   -- (yield)
    respond connection k                  -- (loop)

{-
  case message of
    Text   s -> respond connection s
    Binary s -> respond connection s

server = do
  WS.runServer "0.0.0.0" 9160 

meow :: WS.Connection -> IO ()
meow conn = forever $ do
    msg <- WS.receiveData conn
    WS.sendTextData conn $ msg `T.append` ", meow"

app :: Application
app = WAI.websocketsOr WS.defaultConnectionOptions wsApp backupApp
  where
    wsApp :: ServerApp
    wsApp pending_conn = do
        conn <- WS.acceptRequest pending_conn
        sendTextData conn "Hello, client!"

    backupApp :: Application
    backupApp = undefined
-}
