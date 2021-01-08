module Main where

import Core
import Types
import Commands

import System.IO


test :: CommandFunc
test _ _ = Just "I am working"

myName :: CommandFunc
myName author _ = Just $ "You are " ++ author ++ ", and that is a lovely name :D"

addNumbers :: CommandFunc
addNumbers _ xs = do
    nums <- mapM read' xs
    return . show $ sum nums

commands = fromList [ ("test", test)
                    , ("name", myName)
                    , ("add", addNumbers)]

prefix :: String
prefix = "!"

main :: IO ()
main = do
    -- setup bot
    tokenFile <- openFile "token.txt" ReadMode
    token <- hGetContents tokenFile
    let theRealVivax = Login "therealvivax" token
    sock <- connectToTwitch theRealVivax
    hClose tokenFile

    -- connect to upjump
    connectToChannel sock "upjump"
    sendMessage sock "upjump" "I am online"

    -- start processing commands
    eventLoop sock $ commandHandler prefix commands
    close sock
