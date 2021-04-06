module Example.DomRandom where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Random (random)
import Graphics.Canvas (Context2D, arc, fillPath, getCanvasElementById,
                        getContext2D, setFillStyle, setStrokeStyle, strokePath)
import Math as Math
import Partial.Unsafe (unsafePartial)
import Web.DOM.Document (toParentNode)
import Web.DOM.Element (toEventTarget)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)

main :: Effect Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  doc <- map (toParentNode <<< toDocument) (document =<< window)
  Just node <- querySelector (QuerySelector "#canvas") doc

  setFillStyle ctx "#F00"
  setStrokeStyle ctx "#000"

  clickListener <- eventListener \_ -> do
    drawRandomCircle ctx

  addEventListener (EventType "click") clickListener true (toEventTarget node)

drawCircle :: Context2D -> Number -> Number -> Number -> Effect Unit
drawCircle ctx x y r = let
  path = arc ctx
         { x      : x
         , y      : y
         , radius : r
         , start  : 0.0
         , end    : Math.tau
         }
  in do fillPath   ctx path
        strokePath ctx path

drawRandomCircle :: Context2D -> Effect Unit
drawRandomCircle ctx = do
  x <- random
  y <- random
  r <- random
  drawCircle ctx (x * 600.0) (y * 600.0) (r * 50.0)
