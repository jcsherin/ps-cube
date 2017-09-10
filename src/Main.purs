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
import Data.Maybe (Maybe(..), fromJust)
import Geometry.Cube (faces, rotateX, rotateY)
import Geometry.Point (orthographic)
import Graphics.Canvas (CANVAS, Context2D, beginPath, clearRect, closePath, getCanvasElementById, getContext2D, lineTo, moveTo, setLineWidth, setStrokeStyle, stroke, translate)
import Math (round)
import Partial.Unsafe (unsafePartial)

animate :: forall e. Context2D -> Eff (console :: CONSOLE, canvas :: CANVAS, dom :: DOM | e) Unit
animate ctx = void do
  drawCube ctx
  window >>= requestAnimationFrame (animate ctx)

drawCube :: forall e. Context2D -> Eff (console :: CONSOLE, canvas :: CANVAS, dom :: DOM | e) Unit
drawCube ctx = void $ do
  let rotateX' = rotateX 0.15
  let rotateY' = rotateY (-0.15)
  let projected = map orthographic <$> map rotateX' <$> map rotateY' <$> faces 80.0
  for_ projected \face -> do

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
  l <- readRef dragged
  x <- readRef prevX
  y <- readRef prevY
  log $ show l <> ", " <> show x <> ", " <> show y

rotate :: forall e. Ref Boolean -> Ref Int -> Ref Int -> MouseEvent -> Eff (ref :: REF, console :: CONSOLE | e) Unit
rotate dragged prevX prevY e = do
  x <- readRef prevX
  y <- readRef prevY
  let x' = clientX e
  let y' = clientY e
  log $ show x <> " -> " <> show x'
  modifyRef prevX (\x -> x')
  modifyRef prevY (\y -> y')

release :: forall e. Ref Boolean -> MouseEvent -> Eff (ref :: REF, console :: CONSOLE | e) Unit
release dragged e = do
  modifyRef dragged (\l -> false)
  l <- readRef dragged
  log $ show l

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

  _ <- clearRect ctx { x: 0.0, y: 0.0, w: 600.0, h: 400.0}
  _ <- translate { translateX: 300.0, translateY: 200.0} ctx
  _ <- setStrokeStyle "#ff0000" ctx
  _ <- setLineWidth 1.0 ctx

  dragged <- newRef false
  prevX <- newRef 0
  prevY <- newRef 0

  addEventHandler "mousedown" $ lock dragged prevX prevY
  addEventHandler "mousemove" $ rotate dragged prevX prevY
  addEventHandler "mouseup" $ release dragged

  animate ctx
