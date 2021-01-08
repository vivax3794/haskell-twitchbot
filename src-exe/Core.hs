module Core ( ResponseFunc
            , connectToTwitch
            , connectToChannel
            , sendMessage
            , eventLoop
            , close) where

import Types
import Formating
import Parsing

import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

import Data.ByteString.Char8 (pack, unpack)
import Control.Monad
import Data.List

type ResponseFunc = Message -> Maybe String

sendAll' :: Socket -> String -> IO ()
sendAll' sock s = sendAll sock $ pack s

recv' :: Socket -> IO String
recv' sock = unpack <$> recv sock 1024

connectToTwitch :: Login -> IO Socket
connectToTwitch login = do
    let hints = defaultHints {addrFlags = [AI_NUMERICSERV]}
    addr <- head <$> getAddrInfo (Just hints) (Just "irc.chat.twitch.tv") (Just "6667")
    sock <- openSocket addr
    connect sock (addrAddress addr)
    sendAll' sock $ formatLogin login

    return sock

connectToChannel :: Socket -> Channel -> IO ()
connectToChannel sock channel = sendAll' sock $ formatJoin channel

sendMessage :: Socket -> Channel -> String -> IO ()
sendMessage sock channel msg = sendAll' sock $ formatMessage channel msg

inspect :: (Show a) => IO a -> IO a
inspect val = do
    a <- val
    print a
    return a

eventLoop :: Socket -> ResponseFunc -> IO ()
eventLoop sock f = forever $ mapM_ handleMessage . messages =<< recv' sock
    where isMessage = isInfixOf "PRIVMSG"
          messages = map parseMessage . filter isMessage . lines
          handleMessage message@(Message {channel = channel}) = case f message of
                                                            Just res -> sendMessage sock channel res
                                                            Nothing  -> return ()
