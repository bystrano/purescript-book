module Test.MySolutions where

import Prelude

import Data.Array (length, nub, nubByEq, nubEq)
import Data.Array as A
import Data.Foldable (class Foldable, foldMap, foldl, foldr, maximum)
import Data.Generic.Rep (class Generic)
import Data.Hashable (class Hashable, hashCode, hashEqual)
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Data.Newtype (class Newtype, over2, wrap)
import Data.Show.Generic (genericShow)

newtype Point
  = Point
  { x :: Number
  , y :: Number
  }

instance showPoint :: Show Point where
  show (Point p) = "(" <> show p.x <> ", " <> show p.y <> ")"

newtype Complex
  = Complex
    { real :: Number
    , imaginary :: Number
    }

instance showComplex :: Show Complex where
  show (Complex c)
    | c.imaginary >= 0.0 = show c.real <> "+" <> show c.imaginary <> "i"
    | otherwise          = show c.real <>        show c.imaginary <> "i"

derive instance eqComplex :: Eq Complex

derive instance newtypeComplex :: Newtype Complex _

instance semiringComplex :: Semiring Complex where
  add = over2 Complex add
  mul = over2 Complex
        \ { real: r1, imaginary: i1 } { real: r2, imaginary: i2 }
        -> { real: r1 * r2 - i1 * i2
           , imaginary: r1 * i2 + i1 * r2
           }
  zero = wrap zero
  one = wrap one

instance ringComplex :: Ring Complex where
  sub = over2 Complex sub

data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String

derive instance genericShape :: Generic Shape _

instance showShape :: Show Shape where
  show = genericShow

data NonEmpty a = NonEmpty a (Array a)

instance eqNonEmpty :: Eq a => Eq (NonEmpty a) where
  eq (NonEmpty e1 a1) (NonEmpty e2 a2) = e1 == e2 && a1 == a2

instance showNonEmpty :: Show a => Show (NonEmpty a) where
  show (NonEmpty e a) = "(NonEmpty " <> show e <> " " <> show a <> ")"

instance semigroupNonEmpty :: Semigroup (NonEmpty a) where
  append (NonEmpty e1 a1) (NonEmpty e2 a2) = (NonEmpty e1 (a1 <> [e2] <> a2))

instance functorNonEmpty :: Functor NonEmpty where
  map f (NonEmpty e a) = (NonEmpty (f e) (map f a))

data Extended a = Infinite | Finite a

derive instance eqExtended :: Eq a => Eq (Extended a)

instance ordExtended :: Ord a => Ord (Extended a) where
  compare Infinite   Infinite   = EQ
  compare (Finite _) Infinite   = LT
  compare Infinite   (Finite _) = GT
  compare (Finite a) (Finite b) = compare a b

instance foldableNonEmpty :: Foldable NonEmpty where
  foldr func acc (NonEmpty value arr) = A.foldr func acc ([ value ] <> arr)
  foldl func acc (NonEmpty value arr) = A.foldl func acc ([ value ] <> arr)
  foldMap func (NonEmpty val arr) = A.foldMap func ([ val ] <> arr)

data OneMore f a = OneMore a (f a)

instance foldableOneMore :: Foldable f => Foldable (OneMore f) where
  foldr func acc (OneMore val more) = func val $ foldr func acc more
  foldl func acc (OneMore val more) = foldl func (func acc val) more
  foldMap func (OneMore val more) = (func val) <> (foldMap func more)

derive instance eqPoint :: Eq Point
derive instance eqShape :: Eq Shape

dedupShapes :: Array Shape -> Array Shape
dedupShapes = nubEq

derive instance ordPoint :: Ord Point
derive instance ordShape :: Ord Shape

dedupShapesFast :: Array Shape -> Array Shape
dedupShapesFast = nub


unsafeMaximum :: Partial => Array Int -> Int
unsafeMaximum arr = case maximum arr of
  (Just max) -> max


class Monoid m <= Action m a where
  act :: m -> a -> a

newtype Multiply = Multiply Int

instance semigroupMultiply :: Semigroup Multiply where
  append (Multiply n) (Multiply m) = Multiply (n * m)

instance monoidMultiply :: Monoid Multiply where
  mempty = Multiply 1

instance actionMultiplyInt :: Action Multiply Int where
  act :: Multiply -> Int -> Int
  act (Multiply m) i = m * i


instance actionMultiplyString :: Action Multiply String where
  act (Multiply n) s = power s n


instance actionArray :: Action m a => Action m (Array a) where
  act :: m -> Array a -> Array a
  act m arr = map (act m) arr


newtype Self m = Self m

derive instance eqMultiply :: Eq Multiply
derive newtype instance showMultiply :: Show Multiply

derive newtype instance eqSelf :: Eq m => Eq (Self m)
derive newtype instance showSelf :: Show m => Show (Self m)

instance actionSelf :: Monoid m => Action m (Self m) where
  act m (Self n) = Self (append m n)


arrayHasDuplicates :: forall a. Hashable a => Array a -> Boolean
arrayHasDuplicates arr =
  let hashAndValueEqual a b = hashEqual a b && a == b
  in length arr /= (length $ nubByEq hashAndValueEqual arr)


newtype Hour = Hour Int

instance eqHour :: Eq Hour where
  eq (Hour n) (Hour m) = mod n 12 == mod m 12

instance hashHour :: Hashable Hour where
  hash (Hour n) = hashCode $ mod n 12
