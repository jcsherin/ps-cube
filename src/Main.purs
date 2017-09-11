module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, newRef, readRef)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.Event.EventTarget (EventListener, addEventListener, eventListener)
import DOM.Event.MouseEvent (MouseEvent, clientX, clientY, eventToMouseEvent)
import DOM.Event.Types (EventType(..))
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToParentNode)
import DOM.HTML.Window (document, requestAnimationFrame)
import DOM.Node.ParentNode (QuerySelector(..), querySelector)
import DOM.Node.Types (elementToEventTarget)
import Data.Array (head, tail)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromJust)
import Geometry.Cube (faces, rotateX, rotateY)
import Geometry.Point (Point, orthographic)
import Graphics.Canvas (CANVAS, Context2D, beginPath, clearRect, closePath, getCanvasElementById, getContext2D, lineTo, moveTo, setLineWidth, setStrokeStyle, stroke, translate)
import Math (abs, pi, round)
import Partial.Unsafe (unsafePartial)

decelerate :: Number -> Number -> Number
decelerate rad factor = if (abs rad) > 0.009
  then rad * factor
  else 0.0

animate :: forall e. Context2D -> Ref Number -> Ref Number -> Ref Boolean -> Array (Array Point) -> Eff (ref :: REF, console :: CONSOLE, canvas :: CANVAS, dom :: DOM | e) Unit
animate ctx deltaX deltaY dragged cube = void do
  _ <- clearRect ctx { x: -300.0, y: -200.0, w: 600.0, h: 400.0}

  dx <- readRef deltaX
  dy <- readRef deltaY
  drg <- readRef dragged
  let rotated = map (rotateX dy) <$> map (rotateY dx) <$> cube

  drawCube ctx rotated

  modifyRef deltaX (\x -> decelerate x 0.95)
  modifyRef deltaY (\y -> decelerate y 0.95)

  window >>= requestAnimationFrame (animate ctx deltaX deltaY dragged rotated)

drawCube :: forall e. Context2D -> Array (Array Point) -> Eff (console :: CONSOLE, canvas :: CANVAS, dom :: DOM | e) Unit
drawCube ctx cube = void $ do
  for_ (map orthographic <$> cube) \face -> do

    let hd = unsafePartial $ fromJust $ head face
    _ <- beginPath ctx
    _ <- moveTo ctx (round hd.x) (round $ -hd.y)

    let tl = unsafePartial $ fromJust $ tail face
    for_ tl \p -> do
      lineTo ctx (round p.x) (round $ -p.y)

    _ <- closePath ctx
    stroke ctx

lock :: forall e. Ref Boolean -> Ref Int -> Ref Int -> MouseEvent -> Eff (ref :: REF, console :: CONSOLE | e) Unit
lock dragged prevX prevY e = do
  modifyRef dragged (\l -> true)
  modifyRef prevX (\x -> clientX e)
  modifyRef prevY (\y -> clientY e)

rotate :: forall e. Ref Boolean -> Ref Int -> Ref Int -> Ref Number -> Ref Number -> MouseEvent -> Eff (ref :: REF, console :: CONSOLE | e) Unit
rotate dragged prevX prevY deltaX deltaY e = do
  drg <- readRef dragged
  if drg
    then do
      px <- readRef prevX
      py <- readRef prevY
      modifyRef deltaX (\x -> toNumber (clientX e - px) * pi / 180.0)
      modifyRef deltaY (\y -> toNumber (- (clientY e - py)) * pi / 180.0)
      modifyRef prevX (\x -> clientX e)
      modifyRef prevY (\y -> clientY e)
    else pure unit

release :: forall e. Ref Boolean -> MouseEvent -> Eff (ref :: REF, console :: CONSOLE | e) Unit
release dragged e = do
  modifyRef dragged (\l -> false)

makeEventHandler :: forall e. (MouseEvent -> Eff e Unit) -> EventListener e
makeEventHandler on = eventListener (\ev -> do
  case (runExcept $ eventToMouseEvent ev) of
    Right e -> on e
    Left err -> pure unit
  )

addEventHandler :: forall e. Partial => String -> (MouseEvent -> Eff (dom :: DOM |e) Unit) -> Eff (dom :: DOM | e) Unit
addEventHandler e f = do
  doc <- window >>= document
  Just elem <- querySelector (QuerySelector "canvas") (htmlDocumentToParentNode doc)

  addEventListener (EventType e) (makeEventHandler f) false (elementToEventTarget elem)

main :: forall e. Partial => Eff (ref :: REF, console :: CONSOLE, canvas :: CANVAS, dom :: DOM | e) Unit
main = void do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  _ <- translate { translateX: 300.0, translateY: 200.0} ctx
  _ <- setStrokeStyle "#ff0000" ctx
  _ <- setLineWidth 1.0 ctx

  dragged <- newRef false
  prevX <- newRef 0
  prevY <- newRef 0
  deltaX <- newRef 0.0
  deltaY <- newRef 0.0

  addEventHandler "mousedown" $ lock dragged prevX prevY
  addEventHandler "mousemove" $ rotate dragged prevX prevY deltaX deltaY
  addEventHandler "mouseup" $ release dragged

  let cube = (map $ rotateX (-0.17)) <$> (map $ rotateY 0.17) <$> faces 100.0
  animate ctx deltaX deltaY dragged cube
