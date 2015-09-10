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


loop = forever $ threadDelay (1000 * 1000)


updateWorldStateMVar worldStateMVar playerBlobsMVar (Client.WorldUpdate worldState) = do
    swapMVar worldStateMVar worldState
    --print $ show worldState
    return ()
updateWorldStateMVar worldStateMVar playerBlobsMVar (Client.OwnsBlob blobId) = do
    print $ "Player blob id " ++ (show blobId)
    swapMVar playerBlobsMVar [blobId]
    return ()
updateWorldStateMVar _ _ _ = return ()


updateLeaderboardMVar leaderboardMVar (Client.FFALeaderboard leaderboard) = do
    swapMVar leaderboardMVar leaderboard
    return ()
updateLeaderboardMVar _ _ = return ()


getPlayerBlobData worldStateMVar playerBlobsMVar = do
    ws <- readMVar worldStateMVar
    pbs <- readMVar playerBlobsMVar

    if length pbs == 0
        then return Nothing
        else do
            let pb = pbs !! 0

            let players = filter (\p -> (==) (Client.player_id p) pb) (Client.players ws)

            if length players == 0
                then do
                    print "Player is dead!"
                    return Nothing
                else return $ Just $ players !! 0

sendPlayerDirection conn Nothing x y = return ()
sendPlayerDirection conn (Just player) x y = do
    let msg = Client.set_direction_msg x y (Client.player_id player)
    print $ "Sending direction" ++ (show msg)
    withFile "/home/przemek/haskell-ogar-out" WriteMode $ \fh ->
        BSC.hPutStr fh msg
    WS.sendBinaryData conn msg


app :: WS.ClientApp ()
app conn = do
    putStrLn "Connected!"

    let nickMsg = Client.nick_msg "mynick"
    WS.sendTextData conn nickMsg

    playerBlobs <- newMVar ([] :: [Client.BlobId])
    leaderboard <- newMVar $ Client.Leaderboard {}
    worldState <- newMVar $ Client.defaultWorldState

    -- thread that constantly reads messages from server
    _ <- forkIO $ forever $ do
        msg_ <- WS.receiveData conn
        let msg = Client.read_message msg_
        liftIO $ updateWorldStateMVar worldState playerBlobs msg
        liftIO $ updateLeaderboardMVar leaderboard msg
        --liftIO $ print $ show msg

    -- thread that tracks player's data
    _ <- forkIO $ forever $ do
        player <- liftIO $ getPlayerBlobData worldState playerBlobs
        print $ "Player: " ++ (show player)
        sendPlayerDirection conn player 0 0
        threadDelay (1000 * 1000)

    loop

main :: IO ()
main = do
    NS.withSocketsDo $ WS.runClient "127.0.0.1" 443 "/" app
