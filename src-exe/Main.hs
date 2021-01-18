module Main where

import Core
import Types
import Commands
import Math

import System.IO

prefix :: String
prefix = "!"

targetChannel :: Channel
targetChannel = "arcticspacefox"

test :: CommandFunc
test _ _ = Just "I am working"

myName :: CommandFunc
myName author _ = Just $ "You are " ++ author ++ ", and that is a lovely name :D"

addNumbers :: CommandFunc
addNumbers _ xs = show . sum <$> mapM read' xs

fac :: CommandFunc
fac _ (x:_) = do
    x' <- read' x :: Maybe Integer
    if x' > 100
    then Just "input to large"
    else Just $ show . foldl (*) 1 $ [1..x']
fac _ _ = Just "missing argument"

math :: CommandFunc
math _ xs = show <$> (eval =<< mapM parseWord xs)

best _ _ = Just $ "the best twitch streamer is of course " ++ targetChannel ++ "! there is NOBODY else better than them!"

commands = fromList [ ("test", test)
                    , ("name", myName)
                    , ("add", addNumbers)
                    , ("fac", fac)
                    , ("best", best)
                    , ("math", math)]




main :: IO ()
main = do
    -- setup bot
    tokenFile <- openFile "token.txt" ReadMode
    token <- hGetContents tokenFile
    let theRealVivax = Login "therealvivax" token
    sock <- connectToTwitch theRealVivax
    hClose tokenFile

    -- connect to targetChannel
    connectToChannel sock targetChannel
    sendMessage sock targetChannel "I am online (and i am a bot)"

    -- start processing commands
    eventLoop sock $ commandHandler prefix commands
    close sock
