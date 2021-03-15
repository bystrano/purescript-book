module Test.MySolutions where

import Prelude

import ChapterExamples (Amp(..), Person, Volt(..))
import Data.Maybe (Maybe(..))
import Data.Picture (Shape(..), origin)
import Math (pi)

factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n - 1)

binomial :: Int -> Int -> Int
binomial _ 0 = 1
binomial 0 _ = 0
binomial n k | n < k = 0
             | otherwise = factorial n / (factorial k * factorial (n - k))

pascal :: Int -> Int -> Int
pascal _ 0 = 1
pascal 0 _ = 0
pascal n k = pascal (n - 1) k + pascal (n - 1) (k - 1)

sameCity :: Person -> Person -> Boolean
sameCity { address: { city: c1 } } { address: { city: c2 } } =
  c1 == c2

fromSingleton :: forall a. a -> Array a -> a
fromSingleton _       [singleton] = singleton
fromSingleton default _           = default

circleAtOrigin :: Shape
circleAtOrigin = Circle origin 10.0

doubleScaleAndCenter :: Shape -> Shape
doubleScaleAndCenter (Circle c r) =
  (Circle origin (2.0 * r))
doubleScaleAndCenter (Rectangle c a b) =
  (Rectangle origin (2.0 * a) (2.0 * b))
doubleScaleAndCenter (Line { x: x1, y: y1 } { x: x2, y: y2 }) =
  (Line { x: x1 - x2, y: y1 - y2} { x: x2 - x1, y: y2 - y1 })
doubleScaleAndCenter (Text p s) =
  (Text origin s)

shapeText :: Shape -> Maybe String
shapeText (Text _ s) = Just s
shapeText _          = Nothing

newtype Watt = Watt Number

calculateWattage :: Amp -> Volt -> Watt
calculateWattage (Amp a) (Volt v) = Watt(a * v)

area :: Shape -> Number
area (Circle _ r)      = pi * r * r
area (Rectangle _ a b) = a * b
area _                 = 0.0
