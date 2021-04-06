module Data.GameEnvironment where

type PlayerName = String

newtype GameEnvironment = GameEnvironment
  { playerName    :: PlayerName
  , cheatMode     :: Boolean
  , debugMode     :: Boolean
  }

gameEnvironment :: PlayerName -> Boolean -> Boolean -> GameEnvironment
gameEnvironment playerName cheatMode debugMode = GameEnvironment
  { playerName    : playerName
  , cheatMode     : cheatMode
  , debugMode     : debugMode
  }
