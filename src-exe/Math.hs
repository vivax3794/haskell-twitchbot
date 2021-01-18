module Math (parseWord, eval) where

import Commands (read')
import qualified Data.Map.Strict as Map
import Data.Char
import Control.Monad


type OperatorFunc = (Maybe Double -> Maybe Double -> Maybe Double)
data Instruction = Number (Maybe Double) | Operator OperatorFunc

divChecked :: OperatorFunc
divChecked Nothing _ = Nothing
divChecked _ Nothing = Nothing
divChecked (Just x) (Just y)
    | y == 0 = Nothing
    | otherwise = Just $ x / y



operators :: Map.Map String Instruction
operators = Map.fromList [ ("*", Operator $ liftM2 (*))
                          , ("/", Operator divChecked)
                          , ("+", Operator $ liftM2 (+))
                          , ("-", Operator $ liftM2 (-))
                          , ("^", Operator $ liftM2 (**))]

parseWord :: String -> Maybe Instruction
parseWord w
    | all (\c -> isDigit c || c == '.') w = Just . Number . read' $ w
    | otherwise     = Map.lookup w operators

eval :: [Instruction] -> Maybe Double
eval ((Number x):[]) = x
eval ((Number x):(Operator f):(Number y):xs) = eval $ (Number (f x y)) : xs
eval _ = Nothing
