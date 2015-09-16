{-# LANGUAGE OverloadedStrings #-}

module Lib.Client where

import Control.Monad (replicateM, Monad(..))
import Control.Monad.Loops (untilM)

import qualified Data.Binary.Get as DB
import qualified Data.Binary.Put as DBP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List.Split as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word (Word8, Word16, Word32)

import System.IO


slice from to xs = take (to - from + 1) (drop from xs)


wordToInt w = fromIntegral(w) :: Int


readNullTerminatedString :: DB.Get T.Text
readNullTerminatedString = do
    c1 <- DB.getWord8
    c2 <- DB.getWord8
    if c1 + c2 == 0
        then return ""
        else do
            ws <- readNullTerminatedString
            return $ T.append (T.decodeUtf16LE $ BS.pack [c1, c2]) ws


wordsToInt :: [Word8] -> Int
wordsToInt [] = 0
wordsToInt (w:ws) = (wordToInt w) + 256*(wordsToInt ws)

wordsToIntLE :: [Word8] -> Int
wordsToIntLE = wordsToInt . reverse


type PlayerId = Int
type PlayerName = T.Text
type BlobId = Int


data Eat = Eat {
        eater_id :: PlayerId,
        victim_id :: PlayerId
    }
    deriving (Show)


data Player = Player {
        player_id :: PlayerId
      , player_x :: Int
      , player_y :: Int
      , player_mass :: Int
      , player_color_r :: Int
      , player_color_g :: Int
      , player_color_b :: Int
      , player_flags :: Int
      , player_name :: PlayerName
    }
    deriving (Show)


data LeaderboardPlayer = LeaderboardPlayer {
        leaderboard_player_id :: PlayerId,
        leaderboard_name :: PlayerName
    }
    deriving (Show)


data WorldState = WorldState {
        cnt_eats :: Int
      , eats :: [Eat]
      , players :: [Player]
      , cnt_nodes_to_remove :: Int
    }
    deriving (Show)

defaultWorldState = WorldState {
    cnt_eats = 0
  , eats = []
  , players = []
  , cnt_nodes_to_remove = 0
}


data Leaderboard = Leaderboard {
        player_leaderboard_data :: [LeaderboardPlayer]
    }
    deriving (Show)


data Message = WorldUpdate WorldState | FFALeaderboard Leaderboard | OwnsBlob BlobId
    deriving (Show)


-- Client -> Server messages
connection_start_msg :: BS.ByteString
connection_start_msg = BS.pack [255, 0]

nick_msg :: String -> BS.ByteString
nick_msg nick = zippedNick
    where
        byteNick = BSC.pack nick
        zero = BS.singleton 0
        zippedNick = BS.append zero (T.encodeUtf16LE $ T.pack nick)

set_direction_msg :: Int -> Int -> BlobId -> BS.ByteString
set_direction_msg x y blobId = BSL.toStrict $ DBP.runPut $ do
    DBP.putWord8 16
    DBP.putWord32le (fromIntegral(x) :: Word32)
    DBP.putWord32le (fromIntegral(y) :: Word32)
    DBP.putWord32le (fromIntegral(blobId) :: Word32)
    --DBP.putWord32le 0  -- TODO: Undocumented, but required by Ogar (message size must be 13) ?


-- Server -> Client messages
read_message :: BS.ByteString -> Message
read_message bs
    | BS.head bs == 16    = WorldUpdate (readWorldState $ BS.tail bs)
    | BS.head bs == 32    = OwnsBlob (readBlobId $ BS.tail bs)
    | BS.head bs == 49    = FFALeaderboard (readFFALeaderboard $ BS.tail bs)
    | otherwise           = error $ "Unknown opcode " ++ (show $ BS.head bs)


-- reverse-engineer Ogar's packet/UpdateNode.js
readWorldState :: BS.ByteString -> WorldState
readWorldState bs = WorldState {
        cnt_eats = cnt_eats
      , eats = eats
      , players = players
      , cnt_nodes_to_remove = cnt_nodes_to_remove
    }
    where
    (cnt_eats, eats, players, cnt_nodes_to_remove) = DB.runGet worldStateDeserializer $ BSL.fromStrict bs


-- reverse-engineer Ogar's packet/UpdateLeaderboard.js
readFFALeaderboard :: BS.ByteString -> Leaderboard
readFFALeaderboard bs = Leaderboard {
        player_leaderboard_data = players
    }
    where
        players = DB.runGet readFFALeaderboardDeserializer $ BSL.fromStrict bs


readBlobId :: BS.ByteString -> BlobId
readBlobId bs = DB.runGet readBlobIdDeserializer $ BSL.fromStrict bs


-- DESERIALIZERS

worldStateDeserializer :: DB.Get (Int, [Eat], [Player], Int)
worldStateDeserializer = do
    cnt_eats_ <- DB.getWord16le
    let cnt_eats_int = wordToInt(cnt_eats_)
    -- TODO: add eats parsing
    eats <- parseEats cnt_eats_int
    players <- playerDeserializer
    -- TODO: nodes to remove
    cnt_nodes_to_remove_ <- DB.getWord32le
    return (
            cnt_eats_int
          , eats
          , players
          , wordToInt(cnt_nodes_to_remove_)
        )

parseEats :: Int -> DB.Get [Eat]
parseEats 0 = return []
parseEats n = do
    eater_id <- DB.getWord32le
    victim_id <- DB.getWord32le

    eats <- parseEats (n - 1)

    return (Eat {
                eater_id = wordToInt(eater_id)
              , victim_id = wordToInt(victim_id)
            }:eats)

playerDeserializer :: DB.Get [Player]
playerDeserializer = do
    player_id_ <- DB.getWord32le
    if player_id_ == 0
        then return []
        else do
            player_x_ <- DB.getWord32le
            player_y_ <- DB.getWord32le
            player_mass_ <- DB.getWord16le
            player_color_r_ <- DB.getWord8
            player_color_g_ <- DB.getWord8
            player_color_b_ <- DB.getWord8
            player_flags_ <- DB.getWord8
            player_name_ <- readNullTerminatedString
            rest <- playerDeserializer
            return $ (Player {
                player_id = wordToInt(player_id_)
              , player_x = wordToInt(player_x_)
              , player_y = wordToInt(player_y_)
              , player_mass = wordToInt(player_mass_)
              , player_color_r = wordToInt(player_color_r_)
              , player_color_g = wordToInt(player_color_g_)
              , player_color_b = wordToInt(player_color_b_)
              , player_flags = wordToInt(player_flags_)
              , player_name = player_name_
            }:rest)



readFFALeaderboardDeserializer :: DB.Get [LeaderboardPlayer]
readFFALeaderboardDeserializer = do
    player_cnt_ <- DB.getWord32le
    let player_cnt = wordToInt(player_cnt_)

    readFFALeaderboardPlayers player_cnt

readFFALeaderboardPlayers :: Int -> DB.Get [LeaderboardPlayer]
readFFALeaderboardPlayers 0 = return []
readFFALeaderboardPlayers n = do
    leaderboard_player_id <- DB.getWord32le
    leaderboard_name <- readNullTerminatedString

    rest <- readFFALeaderboardPlayers (n - 1)

    return (LeaderboardPlayer {
            leaderboard_player_id = wordToInt(leaderboard_player_id)
          , leaderboard_name = leaderboard_name
        }:rest)


readBlobIdDeserializer :: DB.Get BlobId
readBlobIdDeserializer = do
    blob_id <- DB.getWord32le
    return $ wordToInt blob_id
