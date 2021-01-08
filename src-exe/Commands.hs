module Commands ( CommandFunc
                , CommandTable
                , fromList
                , combine
                , commandHandler
                , read') where


import Types
import Core
import Parsing

import qualified Data.Map.Strict as Map


type CommandFunc  = User -> [String] -> Maybe String
type CommandTable = Map.Map String CommandFunc

fromList :: [(String, CommandFunc)] -> CommandTable
fromList = Map.fromList

combine :: CommandTable -> CommandTable -> CommandTable
combine = Map.union

commandHandler :: String -> CommandTable -> ResponseFunc
commandHandler prefix tabel message = do
    command <- parseCommand prefix message
    func <- Map.lookup (name command) tabel
    return $ case func (author message) (args command) of
                  Just res -> res
                  Nothing  -> "command error"

read' :: Read a => String -> Maybe a
read' x = case reads x of
              [(res, _)] -> Just res
              _          -> Nothing
