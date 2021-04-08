module Example.LSystem where

import Prelude

import Data.Array (concatMap, foldM)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Example.Shapes (renderFilledPath)
import Graphics.Canvas (getCanvasElementById, getContext2D, setFillStyle, setStrokeStyle)
import Math as Math
import Partial.Unsafe (unsafePartial)

lsystem :: forall a m s
         . Monad m
         => Array a
         -> (a -> Array a)
         -> (s -> a -> m s)
         -> Int
         -> s
         -> m s
lsystem init prod interpret n state = go init n
  where
  go s 0 = foldM interpret state s
  go s i = go (concatMap prod s) (i - 1)

data Letter = L | R | F

type Sentence = Array Letter

type State =
  { x :: Number
  , y :: Number
  , theta :: Number
  , path :: Array { x :: Number, y :: Number }
  }

initial :: Sentence
initial = [F, R, R, F, R, R, F, R, R]

productions :: Letter -> Sentence
productions L = [L]
productions R = [R]
productions F = [F, L, F, R, R, F, L, F]

initialState :: State
initialState = { x: 120.0, y: 200.0, theta: 0.0, path: [] }

interpret :: State -> Letter -> Effect State
interpret state L = pure $ state { theta = state.theta - Math.tau / 6.0 }
interpret state R = pure $ state { theta = state.theta + Math.tau / 6.0 }
interpret state F = do
  let x = state.x + Math.cos state.theta * 1.5
      y = state.y + Math.sin state.theta * 1.5
      path = state.path <> [ {x, y} ]
  pure { x, y, theta: state.theta, path }

main :: Effect Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  setStrokeStyle ctx "#000"
  setFillStyle   ctx "#F00"

  path <- _.path <$> lsystem initial productions interpret 5 initialState
  renderFilledPath ctx path
