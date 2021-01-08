module Commands ( CommandFunc
                , CommandTable
                , fromList
                , combine
                , commandHandler) where


import Types
import Core
import Parsing

import qualified Data.Map.Strict as Map


type CommandFunc  = User -> [String] -> String
type CommandTable = Map.Map String CommandFunc

fromList :: [(String, CommandFunc)] -> CommandTable
fromList = Map.fromList

combine :: CommandTable -> CommandTable -> CommandTable
combine = Map.union

commandHandler :: String -> CommandTable -> ResponseFunc
commandHandler prefix tabel message = do
    command <- parseCommand prefix message
    func <- Map.lookup (name command) tabel
    return $ func (author message) (args command)
