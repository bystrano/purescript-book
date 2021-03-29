module Test.MySolutions where

import Prelude

import Control.Monad.Reader (Reader, ask, local, runReader)
import Control.Monad.State (State, execState, get, modify)
import Data.Monoid (power)
import Data.String (joinWith)
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (sequence, traverse)

{- The State monad -}
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

{- The Reader monad -}
type Level = Int

type Doc = Reader Level String

-- exercise 1
line :: String -> Doc
line str = do
  level <- ask
  pure $ power "  " level <> str

-- exercise 2
indent :: Doc -> Doc
indent = local $ (+) 1

-- exercise 3
cat :: Array Doc -> Doc
cat = sequence >=> joinWith "\n" >>> pure
-- cat docs = do
--   strings <- sequence docs
--   pure $ joinWith "\n" strings

-- exercise 4
render :: Doc -> String
render doc = runReader doc 0
