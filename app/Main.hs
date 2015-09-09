module Main where

import qualified Lib.Client as Client

import Control.Concurrent (forkIO, threadDelay, newEmptyMVar, newMVar, putMVar, swapMVar)
import Control.Monad (forever, unless)
import Control.Monad.Trans (liftIO)

import qualified Data.ByteString as BS
import qualified Network.Socket as NS
import qualified Network.WebSockets as WS

import System.IO


import qualified Data.Binary.Get as DB
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL


loop = do
    threadDelay 1000
    loop


updateWorldStateMVar mvar (Client.WorldUpdate worldState) = do
    swapMVar mvar worldState
    return ()
updateWorldStateMVar mvar _ = return ()


app :: WS.ClientApp ()
app conn = do
    putStrLn "Connected!"

    worldState <- newMVar $ Client.WorldState {}

    -- thread that constantly reads messages from server
    _ <- forkIO $ forever $ do
        msg_ <- WS.receiveData conn
        let msg = Client.read_message msg_
        liftIO $ updateWorldStateMVar worldState msg
        liftIO $ print $ show msg

    --msg_ <- WS.receiveData conn
    --let msg = Client.read_message msg_
    --print $ show msg

    loop

    --let nickMsg = Client.nick_msg "my nick"

    --WS.sendTextData conn nickMsg

    --msg <- WS.receiveData conn :: IO BS.ByteString
    --print $ show msg

    --WS.sendTextData conn (Client.connection_start_msg)

main :: IO ()
main = do
    NS.withSocketsDo $ WS.runClient "127.0.0.1" 443 "/" app
