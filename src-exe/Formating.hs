module Formating (formatLogin, formatMessage, formatJoin) where

import Types

ending = "\r\n"

formatLogin :: Login -> String
formatLogin (Login username token) = "PASS " ++ token ++ ending ++ "NICK " ++ username ++ ending

formatMessage :: Channel -> String -> String
formatMessage channel content = "PRIVMSG #" ++ channel ++ " :" ++ content ++ ending

formatJoin :: Channel -> String
formatJoin channel = "JOIN #" ++ channel ++ ending
