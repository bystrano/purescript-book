module Test.MySolutions where

import Prelude

import Control.Parallel (parOneOf, parTraverse)
import Data.Array (filter)
import Data.Either (Either)
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), length, split)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Effect.Aff (Aff, Error, attempt, delay)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Node.Path (FilePath, dirname)
import Test.HTTP (getUrl)

{- Asynchronous PureScript -}

-- Exercise 1
concatenateFiles :: String -> String -> String -> Aff Unit
concatenateFiles inFile1 inFile2 outFile = do
  inStr1 <- readTextFile UTF8 inFile1
  inStr2 <- readTextFile UTF8 inFile2
  outStr <- pure $ inStr1 <> inStr2
  writeTextFile UTF8 outFile outStr

-- exercise 2
concatenateMany :: Array String -> String -> Aff Unit
concatenateMany files outFile = do
  contents <- traverse (readTextFile UTF8) files
  writeTextFile UTF8 outFile (fold contents)

-- exercise 3
countCharacters :: FilePath -> Aff (Either Error Int)
countCharacters file = attempt do
  content <- readTextFile UTF8 file
  pure $ length content

{- An HTTP client -}

-- exercise 1
writeGet :: String -> String -> Aff Unit
writeGet url file = do
  str <- getUrl url
  writeTextFile UTF8 file str

{- Parallel Computations -}

-- exercise 1
concatenateManyParallel :: Array String -> String -> Aff Unit
concatenateManyParallel files outFile = do
  contents <- parTraverse (readTextFile UTF8) files
  writeTextFile UTF8 outFile (fold contents)

-- exercise 2
getWithTimeout :: Number -> String -> Aff (Maybe String)
getWithTimeout ms url =
  parOneOf [ delay (Milliseconds ms) $> Nothing
           , getUrl url <#> pure
           ]

-- exercise 3
recurseFiles :: String -> Aff (Array String)
recurseFiles rootFile =
  let
    prependPath = \f -> dirname rootFile <> "/" <> f
  in do
    contents <- readTextFile UTF8 rootFile
    files    <- pure $ filter (not eq "") $ split (Pattern "\n") contents
    recFiles <- parTraverse recurseFiles $ prependPath <$> files
    pure $ fold recFiles <> [rootFile]
