module Test.MySolutions where

import Prelude

import Data.Function.Uncurried (Fn3)
import Data.Pair (Pair(..))
import Test.Examples (Complex, Quadratic)

{- Calling javascript functions -}

-- exercise 1
foreign import volumeFn :: Fn3 Number Number Number Number

-- exercise 2
foreign import volumeArrow :: Number -> Number -> Number -> Number

{- Passing simple types -}

-- exercise 1
foreign import cumulativeSumsComplex :: Array Complex -> Array Complex

{- Beyond Simple Types -}

-- exercise 1
foreign import quadraticRootsImpl :: (forall a. a -> a -> Pair a) -> Number -> Number -> Number -> Pair Complex

quadraticRoots :: Quadratic -> Pair Complex
quadraticRoots {a, b, c} = quadraticRootsImpl Pair a b c
