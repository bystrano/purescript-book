module Test.MySolutions where

import Prelude

import Control.Apply (lift2)
import Data.AddressBook (Address, PhoneNumber, address)
import Data.AddressBook.Validation (Errors, matches, nonEmpty, validateAddress, validatePhoneNumbers)
import Data.Foldable (foldMap, foldl, foldr)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String.Regex (Regex)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (class Foldable, class Traversable, sequence, traverse)
import Data.Validation.Semigroup (V)


addMaybe :: forall a. Semiring a => Maybe a -> Maybe a -> Maybe a
addMaybe = lift2 add

subMaybe :: forall a. Ring a => Maybe a -> Maybe a -> Maybe a
subMaybe = lift2 sub

mulMaybe :: forall a. Semiring a => Maybe a -> Maybe a -> Maybe a
mulMaybe = lift2 mul

divMaybe :: forall a. EuclideanRing a => Maybe a -> Maybe a -> Maybe a
divMaybe = lift2 div


addApply :: forall a f. Apply f => Semiring a => f a -> f a -> f a
addApply = lift2 add

subApply :: forall a f. Apply f => Ring a => f a -> f a -> f a
subApply = lift2 sub

mulApply :: forall a f. Apply f => Semiring a => f a -> f a -> f a
mulApply = lift2 mul

divApply :: forall a f. Apply f => EuclideanRing a => f a -> f a -> f a
divApply = lift2 div


combineMaybe :: forall a f. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe Nothing = pure Nothing
combineMaybe (Just fa) = map Just fa


stateRegex :: Regex
stateRegex = unsafeRegex "^[a-zA-Z]{2}$" noFlags


nonEmptyRegex :: Regex
nonEmptyRegex = unsafeRegex "[^ \t\n]" noFlags


validateAddressImproved :: Address -> V Errors Address
validateAddressImproved a = ado
  street <- matches "Street" nonEmptyRegex a.street
  city   <- matches "City"   nonEmptyRegex a.city
  state  <- matches "State"  stateRegex    a.state
  in address street city state

{- Traversable Functors -}

-- Exercise 1
data Tree a = Leaf | Branch (Tree a) a (Tree a)

derive instance genericTree :: Generic (Tree a) _

instance showTree :: Show a => Show (Tree a) where
  show t = genericShow t

derive instance eqTree :: Eq a => Eq (Tree a)

-- Exercise 2
instance functorTree :: Functor Tree where
  map :: forall a b. (a -> b) -> Tree a -> Tree b
  map func Leaf = Leaf
  map func (Branch t1 a t2) = Branch (func <$> t1) (func a) (func <$> t2)

instance foldableTree :: Foldable Tree where
  foldr :: forall a b. (a -> b -> b) -> b -> Tree a -> b
  foldr _ acc Leaf = acc
  foldr func acc (Branch t1 v t2) =
    let
      ft2 = foldr func acc t2
      fv  = func v ft2
      ft1 = foldr func fv t1
    in ft1

  foldl :: forall a b. (b -> a -> b) -> b -> Tree a -> b
  foldl _ acc Leaf = acc
  foldl func acc (Branch t1 v t2) =
    let
      ft1 = foldl func acc t1
      fv  = func ft1 v
      ft2 = foldl func fv t2
    in ft2

  foldMap :: forall a m. Monoid m => (a -> m) -> Tree a -> m
  foldMap _ Leaf = mempty
  foldMap func (Branch t1 v t2) =
    (foldMap func t1) <>
    (func v) <>
    (foldMap func t2)

instance traversableTree :: Traversable Tree where
  traverse :: forall a b m. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
  traverse _ Leaf = pure Leaf
  traverse f (Branch t1 v t2) = ado
    mt1 <- traverse f t1
    mv  <- f v
    mt2 <- traverse f t2
    in Branch mt1 mv mt2

  sequence :: forall a m. Applicative m => Tree (m a) -> m (Tree a)
  sequence Leaf = pure Leaf
  sequence (Branch t1 v t2) = ado
    mt1 <- sequence t1
    mv  <- v
    mt2 <- sequence t2
    in Branch mt1 mv mt2

-- exercise 3
traversePreOrder :: forall a b m. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
traversePreOrder _ Leaf = pure Leaf
traversePreOrder f (Branch t1 v t2) = ado
  mv <- f v
  mt1 <- traversePreOrder f t1
  mt2 <- traversePreOrder f t2
  in Branch mt1 mv mt2

-- exercise 4
traversePostOrder :: forall a b m. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
traversePostOrder _ Leaf = pure Leaf
traversePostOrder f (Branch t1 v t2) = ado
  mt1 <- traversePostOrder f t1
  mt2 <- traversePostOrder f t2
  mv <- f v
  in Branch mt1 mv mt2

-- exercise 5
type Person = { firstName :: String
              , lastName :: String
              , homeAddress :: Maybe Address
              , phones :: Array PhoneNumber
              }

validatePersonOptionalAddress :: Person -> V Errors Person
validatePersonOptionalAddress p = ado
  firstName   <- nonEmpty "First Name" p.firstName
  lastName    <- nonEmpty "Last Name" p.lastName
  homeAddress <- traverse validateAddress p.homeAddress
  phones      <- validatePhoneNumbers "Phone numbers" p.phones
  in { firstName, lastName, homeAddress, phones }

-- exercises 6
sequenceUsingTraverse :: forall t a m. Traversable t => Applicative m => t (m a) -> m (t a)
sequenceUsingTraverse t = traverse identity t

-- exercise 7
traverseUsingSequence :: forall a b m t. Traversable t => Applicative m => (a -> m b) -> t a -> m (t b)
traverseUsingSequence f t = sequence (f <$> t)
