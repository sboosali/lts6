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
  WS.runServer "127.0.0.1" 8888 handleConnection

-- handleConnection will be invoked every time a client connects to the server.
-- we need to accept the connection to complete the handshake.
handleConnection pending = do
  connection <- WS.acceptRequest pending
  message <- WS.receiveData connection
  forever $ do
    respond connection message

respond connection message = do
  let response = (message :: T.Text) <> "!!!"
  WS.sendTextData connection response

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
