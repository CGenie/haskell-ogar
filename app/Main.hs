module Main where

import qualified Lib.Client as Client

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, unless)
import Control.Monad.Trans (liftIO)

import qualified Data.ByteString as BS
import qualified Network.Socket as NS
import qualified Network.WebSockets as WS

import System.IO


import qualified Data.Binary.Get as DB
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL


app :: WS.ClientApp ()
app conn = do
    putStrLn "Connected!"

    msg_ <- WS.receiveData conn
    let msg = Client.read_message msg_
    print $ show msg
    withFile "/home/przemek/haskell-ogar-out" WriteMode $ \fh -> do
        BS.hPutStr fh msg_

    msg_ <- WS.receiveData conn
    let msg = Client.read_message msg_
    print $ show msg

    return ()

    --let nickMsg = Client.nick_msg "my nick"

    --WS.sendTextData conn nickMsg

    --msg <- WS.receiveData conn :: IO BS.ByteString
    --print $ show msg

    --WS.sendTextData conn (Client.connection_start_msg)

main :: IO ()
main = do
    NS.withSocketsDo $ WS.runClient "127.0.0.1" 443 "/" app
