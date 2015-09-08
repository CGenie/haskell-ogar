{-# LANGUAGE OverloadedStrings #-}

module Lib.Client where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T

import System.IO


nick_msg :: String -> BS.ByteString
nick_msg nick = zippedNick
    where
        byteNick = BSC.pack nick
        zero = BS.singleton 0
        --zippedNick = BS.foldl' (\acc char -> BS.append acc (BS.pack [0, char])) zero byteNick
        zippedNick = BS.append zero (T.encodeUtf16LE nick)

connection_start_msg :: BS.ByteString
connection_start_msg = BS.pack [255, 0]
