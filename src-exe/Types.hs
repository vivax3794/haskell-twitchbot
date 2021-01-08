module Types where

type User = String
type Channel = String

data Message = Message {author :: User, channel :: Channel, content :: String} deriving (Eq)
data Command = Command {message :: Message, name :: String, args :: [String]} deriving (Eq)
data Login = Login {username :: User, token :: String}

instance Show Message where
    show message = author message ++ ": " ++ content message


instance Show Command where
    show (Command _ name args) = "Command " ++ name ++ " " ++ show args
