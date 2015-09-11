module Main where

import qualified Lib.Client as Client

import Control.Concurrent (forkIO, threadDelay, newEmptyMVar, newMVar, putMVar, readMVar, swapMVar, takeMVar)
import Control.Monad (forever, unless)
import Control.Monad.Trans (liftIO)

import qualified Data.ByteString as BS
import qualified Network.Socket as NS
import qualified Network.WebSockets as WS

import System.IO


import qualified Data.Binary.Get as DB
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL


updateWorldStateMVar worldStateMVar playerBlobMVar (Client.WorldUpdate worldState) = do
    swapMVar worldStateMVar worldState
    return ()
updateWorldStateMVar worldStateMVar playerBlobMVar (Client.OwnsBlob blobId) = do
    print $ "Player blob id " ++ (show blobId)
    putMVar playerBlobMVar blobId
    return ()
updateWorldStateMVar _ _ _ = return ()


updateLeaderboardMVar leaderboardMVar (Client.FFALeaderboard leaderboard) = do
    swapMVar leaderboardMVar leaderboard
    return ()
updateLeaderboardMVar _ _ = return ()


setPlayerBlobData mainPlayerMVar worldStateMVar playerBlobMVar = do
    ws <- readMVar worldStateMVar
    pb <- readMVar playerBlobMVar

    let players = filter (\p -> (==) (Client.player_id p) pb) (Client.players ws)

    if length players == 0
        then do
            print "Player is dead!"
        else do
            putMVar mainPlayerMVar (players !! 0)

sendPlayerDirection conn player x y = do
    let msg = Client.set_direction_msg x y (Client.player_id player)
    print $ "Sending direction" ++ (show msg)
    withFile "/home/przemek/haskell-ogar-out" WriteMode $ \fh ->
        BSC.hPutStr fh msg
    WS.sendBinaryData conn msg


operatePlayer conn mainPlayerMVar worldStateMVar = do
    player <- readMVar mainPlayerMVar
    print $ "Player: " ++ (show player)
    sendPlayerDirection conn player 0 0


app :: WS.ClientApp ()
app conn = do
    putStrLn "Connected!"

    let nickMsg = Client.nick_msg "mynick"
    WS.sendTextData conn nickMsg

    playerBlob <- newEmptyMVar
    mainPlayer <- newEmptyMVar
    leaderboard <- newMVar $ Client.Leaderboard {}
    worldState <- newMVar $ Client.defaultWorldState

    -- thread that constantly reads messages from server
    _ <- forkIO $ forever $ do
        msg_ <- WS.receiveData conn
        let msg = Client.read_message msg_
        liftIO $ updateWorldStateMVar worldState playerBlob msg
        liftIO $ updateLeaderboardMVar leaderboard msg
        --liftIO $ print $ show msg

    -- thread that tracks player's data
    _ <- forkIO $ forever $ do
        setPlayerBlobData mainPlayer worldState playerBlob
        threadDelay (100 * 1000)

    -- thread that operates mainPlayer according to worldState
    _ <- forkIO $ forever $ do
        operatePlayer conn mainPlayer worldState
        threadDelay (100 * 1000)

    forever $ threadDelay (1000 * 1000)


main :: IO ()
main = do
    NS.withSocketsDo $ WS.runClient "127.0.0.1" 443 "/" app
