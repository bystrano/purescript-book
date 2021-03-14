module Test.MySolutions where

import Prelude

import Control.MonadZero (guard)
import Data.Array (cons, filter, foldl, head, length, null, sortBy, tail, (..))
import Data.Int (quot, rem)
import Data.Maybe (Maybe, fromMaybe)
import Data.Path (Path(..), filename, isDirectory)
import Data.String (Pattern(..), joinWith, split)
import Test.Examples (allFiles, factors)

isEven :: Int -> Boolean
isEven n =
  if n == 0
     then true
  else if n == 1
       then false
       else isEven (n - 2)

countEven :: Array Int -> Int
countEven = countEven' 0
  where
    countEven' :: Int -> Array Int -> Int
    countEven' n arr =
      if null arr
         then n
      else
        if isEven $ fromMaybe 0 $ head arr
           then countEven' (n + 1) $ fromMaybe [] $ tail arr
        else countEven' n $ fromMaybe [] $ tail arr

squared :: Array Number -> Array Number
squared = map \x -> (x * x)

keepNonNegative :: Array Number -> Array Number
keepNonNegative = filter (\x -> x >= 0.0)

infix 5 filter as <$?>

keepNonNegativeRewrite :: Array Number -> Array Number
keepNonNegativeRewrite arr = (\x -> x >= 0.0) <$?> arr

isPrime :: Int -> Boolean
isPrime n = if n == 1
               then false
            else (\l -> l == 1) $ length $ factors n

cartesianProduct :: forall ( a :: Type ). Array a -> Array a -> Array (Array a)
cartesianProduct a1 a2 = do
  e1 <- a1
  e2 <- a2
  pure [ e1, e2 ]

triples :: Int -> Array (Array Int)
triples n = do
  a <- 1 .. n
  b <- a .. n
  c <- b .. n
  guard $ a * a + b * b == c * c
  pure [ a, b, c ]

factorize :: Int -> Array Int
factorize n = factorize' 2 n []
  where
  factorize' :: Int -> Int -> Array Int -> Array Int
  factorize' _ 1 result = result

  factorize' divisor dividend result =
    let
      remainder = rem dividend divisor
    in
      if remainder == 0 then
        factorize' (divisor) (quot dividend divisor) (cons divisor result)
      else
        factorize' (divisor + 1) dividend result

allTrue :: Array Boolean -> Boolean
allTrue = foldl (\a b -> a == true && b == true) true

fibTailRec :: Int -> Int
fibTailRec n = fib' n 0 0 1
  where
  fib' :: Int -> Int -> Int -> Int -> Int
  fib' limit count n1 n2 =
    if limit == count then
      n1 + n2
    else
      fib' limit (count + 1) (n1 + n2) n1

reverse :: forall a. Array a -> Array a
reverse = foldl (\arr a -> [a] <> arr) []

onlyFiles :: Path -> Array Path
onlyFiles = allFiles >>> filter (not isDirectory)


matchesFilename :: Path -> String -> Boolean
matchesFilename path fileName = (\s -> s == fileName) $
                                fromMaybe "" $ head $ reverse $
                                split (Pattern "/") (filename path)

dirname :: Path -> String
dirname path = (\s -> s <> "/") $ joinWith "/" $ reverse $
               fromMaybe [] $ tail $ reverse $
               split (Pattern "/") (filename path)

whereIs :: Path -> String -> Maybe Path
whereIs path fileName = head $ whereIs' $ allFiles path
  where
    whereIs' files = do
      file <- files
      guard $ matchesFilename file fileName
      [(Directory (dirname file) [])]

compareBySize :: Path -> Path -> Ordering
compareBySize (File _ size1) (File _ size2) = compare size1 size2
compareBySize (Directory _ _) (Directory _ _) = EQ
compareBySize (Directory _ _) (File _ _) = EQ
compareBySize (File _ _) (Directory _ _) = EQ

largestSmallest :: Path -> Array Path
largestSmallest (File _ _) = []
largestSmallest (Directory _ []) = []
largestSmallest (Directory _ [el]) = [el]
largestSmallest (Directory _ [e1, e2]) = [e1, e2]
largestSmallest dir =
  [
    fromMaybe (File "absurd" 0) $ head $           sortBy compareBySize $ onlyFiles dir,
    fromMaybe (File "absurd" 0) $ head $ reverse $ sortBy compareBySize $ onlyFiles dir
  ]
