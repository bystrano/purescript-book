module Test.MySolutions where

import Prelude

import Control.Monad.Reader (Reader, ask, local, runReader)
import Control.Monad.State (State, evalState, execState, get, gets, modify)
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Int (even)
import Data.Monoid (power)
import Data.Monoid.Additive (Additive(..))
import Data.String (joinWith)
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (sequence, traverse, traverse_)
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested ((/\))
import Type.Function (type ($))

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
cat docs = do
  strings <- sequence docs
  pure $ joinWith "\n" strings

-- exercise 4
render :: Doc -> String
render doc = runReader doc 0

{- The Writer monad -}

-- exercise 1
sumArrayWriter :: Array Int -> Writer (Additive Int) Unit
sumArrayWriter = traverse_ \n -> do
  tell $ Additive n
  pure unit

-- exercise 2
collatzInt :: Int -> Int
collatzInt n = if even n
               then n / 2
               else 3 * n + 1

simpleCollatz :: Int -> Int
simpleCollatz c = fst $ simpleCollatz' (0 /\ c) where
  simpleCollatz' :: Tuple Int Int -> Tuple Int Int
  simpleCollatz' (Tuple s 1) = s /\ 1
  simpleCollatz' (Tuple s n) = simpleCollatz' $ (s + 1) /\ collatzInt n

stateCollatz :: Int -> Int
stateCollatz c = execState (stateCollatz' c) 0 where
  stateCollatz' :: Int -> State Int Int
  stateCollatz' 1 = pure 1
  stateCollatz' n = do
    s <- modify \s -> s + 1
    case evalState (gets $ \_ -> collatzInt n) s of
      m -> stateCollatz' m

collatz :: Int -> Tuple Int (Array Int)
collatz c = unwrapTuple $ runWriter $ collatz' (0 /\ c) where

  collatz' :: Tuple Int Int -> Writer (Array Int) $ Tuple Int Int
  collatz' (Tuple s n) =
    do tell [n]
       case n of
         1 -> pure $ s /\ 1
         m -> collatz' $ (s + 1) /\ collatzInt m

  unwrapTuple (Tuple a b) = Tuple (fst a) b
