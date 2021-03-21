module Test.MySolutions where

import Prelude

import Data.Function.Uncurried (Fn3)
import Test.Examples (Complex)

{- Calling javascript functions -}

-- exercise 1
foreign import volumeFn :: Fn3 Number Number Number Number

-- exercise 2
foreign import volumeArrow :: Number -> Number -> Number -> Number

{- Passing simple types -}

-- exercise 1
foreign import cumulativeSumsComplex :: Array Complex -> Array Complex
