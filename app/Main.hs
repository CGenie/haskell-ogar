module Main where

import qualified Lib.Client as Client

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, unless)
import Control.Monad.Trans (liftIO)

import qualified Data.ByteString as BS
import qualified Network.Socket as NS
import qualified Network.WebSockets as WS


app :: WS.ClientApp ()
app conn = do
    putStrLn "Connected!"


    let nickMsg = Client.nick_msg "my nick"

    WS.sendTextData conn nickMsg

    msg <- WS.receiveData conn :: IO BS.ByteString
    print $ show msg

    WS.sendTextData conn (Client.connection_start_msg)

main :: IO ()
main = do
    NS.withSocketsDo $ WS.runClient "127.0.0.1" 443 "/" app
