module Test.MySolutions where

import Prelude

import Control.Monad.ST (for, run)
import Control.Monad.ST.Ref (modify, new, read)
import Data.Array (head, nub, sort, tail)
import Data.Foldable (foldM)
import Data.Int (toNumber)
import Data.List (List(..), (:))
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Effect.Exception (error, throwException)
import Math (pow)

{- Monads and Applicatives -}

-- exercise 1
third :: forall a. Array a -> Maybe a
third xs = do
  fromSecond <- tail xs
  fromThird  <- tail fromSecond
  head fromThird

-- exercise 2
possibleSums :: Array Int -> Array Int
possibleSums xs = nub $ sort $ foldM (\acc i -> [acc, acc + i]) 0 xs

-- exercise 5
filterM :: forall m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM f Nil      = pure Nil
filterM f (x : xs) = do
  b   <- f x
  xs' <- filterM f xs
  pure if b then (x : xs') else xs'

{- Mutable State -}

-- exercise 1
exceptionDivide :: Number -> Number -> Effect Number
exceptionDivide _ 0.0 = throwException $ error "Division by zero"
exceptionDivide a b   = pure (a / b)

-- exercise 2
estimatePi :: Int -> Number
estimatePi n =
  let
    gterm :: Int -> Number
    gterm = \k ->
      let
        kn = toNumber k
        num = pow (- 1.0) (kn - 1.0)
        denom = 2.0 * kn - 1.0
      in num / denom
  in run do
    ref <- new 0.0
    for 1 n \k ->
      modify (add (gterm k)) ref
    final <- read ref
    pure (final * 4.0)

-- exercise 3
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n =
  let sumT = \(Tuple a b) -> a + b
  in run do
    ref <- new (Tuple 0 1)
    for 1 n \k ->
      modify (\t -> Tuple (snd t) (sumT t)) ref
    final <- read ref
    pure (sumT final)
