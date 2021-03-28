module Test.MySolutions where

import Prelude

import Control.Monad.State (State, execState, get, modify)
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (traverse)


testParens :: String -> Boolean
testParens str =
  let
    updateParenLevel :: Char -> State Int Int
    updateParenLevel '(' = modify \n -> if n >= 0 then n + 1 else n
    updateParenLevel ')' = modify \n -> if n >= 0 then n - 1 else n
    updateParenLevel _   = get

    countParens :: State Int (Array Int)
    countParens = traverse updateParenLevel $ toCharArray str

    parenLevel :: Int
    parenLevel = execState countParens 0

  in parenLevel == 0
