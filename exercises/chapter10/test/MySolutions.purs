module Test.MySolutions where

import Prelude

import Control.Alt ((<|>))
import Control.Apply (lift2)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError(..), decodeJson, encodeJson, jsonParser, printJsonDecodeError)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Either (Either(..))
import Data.Eq.Generic (genericEq)
import Data.Function.Uncurried (Fn3)
import Data.Generic.Rep (class Generic)
-- import Data.Int (fromString)
import Data.Map (Map)
-- import Data.Maybe (Maybe(..))
import Data.Pair (Pair(..))
import Data.Set (Set)
import Data.Show.Generic (genericShow)
-- import Data.String (splitAt)
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
quadraticRoots { a, b, c } = quadraticRootsImpl Pair a b c

{- JSON -}
-- exercise 1
foreign import valuesOfMapJson :: Json -> Json

valuesOfMap :: Map String Int -> Either JsonDecodeError (Set Int)
valuesOfMap = encodeJson >>> valuesOfMapJson >>> decodeJson

-- exercise 2
valuesOfMapGeneric ::
  forall k v.
  Ord k =>
  Ord v =>
  EncodeJson k =>
  EncodeJson v =>
  DecodeJson v =>
  Map k v -> Either JsonDecodeError (Set v)
valuesOfMapGeneric = encodeJson >>> valuesOfMapJson >>> decodeJson

-- exercise 3
foreign import quadraticRootsSetImpl :: Number -> Number -> Number -> Json

quadraticRootsSet :: Quadratic -> Either JsonDecodeError (Set Complex)
quadraticRootsSet { a, b, c } = decodeJson $ quadraticRootsSetImpl a b c

-- exercise 4
newtype WrapPair a
  = WrapPair (Pair a)

instance decodeJsonWrapPair :: DecodeJson a => DecodeJson (WrapPair a) where
  decodeJson j = do
    decoded <- decodeJson j
    case decoded of
      [ a, b ] -> map WrapPair $ lift2 Pair (decodeJson a) (decodeJson b)
      [] -> Left $ AtIndex 0 MissingValue
      [ a ] -> Left $ AtIndex 1 MissingValue
      _ -> Left $ AtIndex 2 $ UnexpectedValue j

foreign import quadraticRootsSafeJson :: Json -> Json

quadraticRootsSafeWrap :: Quadratic -> Either JsonDecodeError (WrapPair Complex)
quadraticRootsSafeWrap = encodeJson >>> quadraticRootsSafeJson >>> decodeJson

quadraticRootsSafe :: Quadratic -> Either JsonDecodeError (Pair Complex)
quadraticRootsSafe = quadraticRootsSafeWrap >>> map (\(WrapPair p) -> p)

-- exercise 5
parseAndDecodeArray2D :: String -> Either String (Array (Array Int))
parseAndDecodeArray2D str = do
  json <- jsonParser str
  case decodeJson json of
    (Left err) -> Left $ printJsonDecodeError err
    (Right res) -> Right res

-- exercise 6
data Tree a
  = Leaf a
  | Branch (Tree a) (Tree a)

derive instance genericTree :: Generic (Tree a) _

instance encodeJsonTree :: EncodeJson a => EncodeJson (Tree a) where
  encodeJson t = genericEncodeJson t

instance decodeJsonTree :: DecodeJson a => DecodeJson (Tree a) where
  decodeJson t = genericDecodeJson t

instance eqTree :: Eq a => Eq (Tree a) where
  eq t = genericEq t

instance showTree :: Show a => Show (Tree a) where
  show t = genericShow t

-- exercise 7
data IntOrString
  = IntOrString_Int Int
  | IntOrString_String String

derive instance genericIntOrString :: Generic IntOrString _

instance eqIntOrString :: Eq IntOrString where
  eq t = genericEq t

instance showIntOrString :: Show IntOrString where
  show t = genericShow t

-- invalid solution, doesn't use the proper datatypes in the json
-- representation, but a custom string representation...

-- instance encodeJsonIntOrString :: EncodeJson IntOrString where
--   encodeJson = encodeJson <<< case _ of
--     (IntOrString_Int i)      -> "Int:" <> show i
--     (IntOrString_String str) -> "Str:" <> str

-- instance decodeJsonIntOrString :: DecodeJson IntOrString where
--   decodeJson json = do
--     s <- decodeJson json
--     intOrStringFromString s where

--       intOrStringFromString :: String -> Either JsonDecodeError IntOrString
--       intOrStringFromString str =
--         let
--           split  = splitAt 4 str
--           prefix = split.before
--           value  = split.after
--         in
--          case prefix of
--            "Int:" -> case fromString value of
--              Just i  -> Right $ IntOrString_Int i
--              Nothing -> Left  $ UnexpectedValue $ encodeJson str
--            "Str:" -> Right $ IntOrString_String value
--            _      -> Left  $ UnexpectedValue $ encodeJson str

instance encodeJsonIntOrString :: EncodeJson IntOrString where
  encodeJson (IntOrString_Int    i) = encodeJson i
  encodeJson (IntOrString_String s) = encodeJson s

instance decodeJsonIntOrString :: DecodeJson IntOrString where
  decodeJson json = IntOrString_Int    <$> decodeJson json <|>
                    IntOrString_String <$> decodeJson json <|>
                    (Left $ TypeMismatch "Not Int nor String")
