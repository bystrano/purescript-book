module Example.Shapes where

import Prelude

import Data.Array (head, tail, zip, (..))
import Data.Foldable (sequence_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit)
import Effect (Effect)
import Graphics.Canvas (Context2D, arc, closePath, fillPath, getCanvasElementById, getContext2D, lineTo, moveTo, rect, setFillStyle, setStrokeStyle, strokePath)
import Math as Math
import Partial.Unsafe (unsafePartial)

translate
  :: forall r
   . Number
  -> Number
  -> { x :: Number, y :: Number | r }
  -> { x :: Number, y :: Number | r }
translate dx dy shape = shape
  { x = shape.x + dx
  , y = shape.y + dy
  }

main :: Effect Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  setFillStyle ctx "#00F"
  setStrokeStyle ctx "#00F"

  strokePath ctx $ rect ctx $ translate (-200.0) (-200.0)
    { x: 250.0
    , y: 250.0
    , width: 100.0
    , height: 100.0
    }

  setFillStyle ctx "#0F0"

  fillPath ctx $ arc ctx $ translate 200.0 200.0
    { x: 300.0
    , y: 300.0
    , radius: 50.0
    , start: 0.0
    , end: Math.tau * 2.0 / 3.0
    }

  setFillStyle ctx "#F00"

  fillPath ctx $ do
    moveTo ctx 300.0 260.0
    lineTo ctx 260.0 340.0
    lineTo ctx 340.0 340.0
    closePath ctx

  -- exercise 2
  strokePath ctx $ do
    rect ctx { x: 250.0
             , y: 250.0
             , width: 100.0
             , height: 100.0
             }
    rect ctx { x: 250.0
             , y: 150.0
             , width: 50.0
             , height: 50.0
             }

  setStrokeStyle ctx "#F00"
  strokePath ctx $ do
    moveTo ctx  30.0 300.0
    lineTo ctx 100.0 300.0
    arc ctx { x: 100.0
            , y: 300.0
            , radius: 70.0
            , start: Math.tau / 4.0
            , end: Math.tau / 2.0
            }

  renderFunction ctx (\f -> { x: 600.0 - 500.0 * f, y: 600.0 - 500.0 * f * f }) 5

-- exercise 3
type Point = { x :: Number, y :: Number }

rotate :: forall a. Array a -> Array a
rotate xs = case head xs of
  Just h -> (fromMaybe [] $ tail xs) <> [ h ]
  _      -> []

toPairs :: forall a. Array a -> Array (Tuple a a)
toPairs xs = zip xs $ rotate xs

line :: Context2D -> Tuple Point Point -> Effect Unit
line ctx (Tuple p1 p2) = do
  moveTo ctx p1.x p1.y
  lineTo ctx p2.x p2.y

renderPath :: Context2D -> Array Point -> Effect Unit
renderPath ctx xs = strokePath ctx $ sequence_ $ line ctx <$> toPairs xs

renderFilledPath :: Context2D -> Array Point -> Effect Unit
renderFilledPath ctx xs =
  fillPath ctx $ do
    sequence_ $ line ctx <$> toPairs xs
    closePath ctx

sample :: (Number -> Point) -> Int -> Array Point
sample f n = f
             <$> (\m -> m / (toNumber n))
             <$> toNumber
             <$> 0 .. (n - 1)

renderFunction :: Context2D -> (Number -> Point) -> Int -> Effect Unit
renderFunction ctx f n = renderPath ctx $ sample f n
