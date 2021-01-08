module Parsing ( isCommand
               , parseCommand
               , parseMessage
               ) where

import Data.List
import Types

splitOnFirst :: Char -> String -> (String, String)
splitOnFirst c (x:xs)
    | c == x    = ("", xs)
    | otherwise = let (a, b) = splitOnFirst c xs
                  in (x:a, b)

isCommand :: String -> String -> Bool
isCommand = isPrefixOf

parseCommand :: String -> Message -> Maybe Command
parseCommand prefix message
    | isCommand prefix (content message) = Just $ Command message (removePrefix $ head args) (tail args)
    | otherwise                = Nothing
    where removePrefix = drop (length prefix)
          args         = words . content $ message

-- :<user>!<user>@<user>.tmi.twitch.tv PRIVMSG #<channel> :This is a sample message
parseMessage :: String -> Message
parseMessage msg = Message user channel content
    where msg' = tail msg
          user = fst $ splitOnFirst '!' msg'
          content = snd $ splitOnFirst ':' msg'
          channel = fst . splitOnFirst ' ' . snd . splitOnFirst '#' $ msg'
