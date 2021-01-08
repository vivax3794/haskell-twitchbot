module Main where

import Core
import Types
import Commands

import System.IO

prefix :: String
prefix = "!"

targetChannel :: Channel
targetChannel = "vivax3794"

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

commands = fromList [ ("test", test)
                    , ("name", myName)
                    , ("add", addNumbers)
                    , ("fac", fac)]


main :: IO ()
main = do
    -- setup bot
    tokenFile <- openFile "token.txt" ReadMode
    token <- hGetContents tokenFile
    let theRealVivax = Login "therealvivax" token
    sock <- connectToTwitch theRealVivax
    hClose tokenFile

    -- connect to upjump
    connectToChannel sock targetChannel
    sendMessage sock targetChannel "I am online"

    -- start processing commands
    eventLoop sock $ commandHandler prefix commands
    close sock
