module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, newRef, readRef)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (requestAnimationFrame)
import Data.Array (head, tail)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromJust)
import Events.Mouse (addMouseEventHandler, handleClick, handleMouseDown, handleMouseMove, handleMouseUp)
import Events.Touch (addTouchEventHandler, handleTouchEnd, handleTouchMove, handleTouchStart)
import Geometry.Cube (rotateX, rotateY, tiltedCube)
import Geometry.Point (Point, Point2D, orthographic)
import Graphics.Canvas (CANVAS, Context2D, beginPath, clearRect, closePath, getCanvasElementById, getContext2D, lineTo, moveTo, setLineWidth, setStrokeStyle, stroke, translate)
import Math (round)
import Model (State, decelerate)
import Partial.Unsafe (unsafePartial)

animate :: forall e. Context2D -> Ref State -> Array (Array Point) -> Eff (ref :: REF, console :: CONSOLE, canvas :: CANVAS, dom :: DOM | e) Unit
animate ctx state cube = void do
  _ <- clearRect ctx { x: -300.0, y: -200.0, w: 600.0, h: 400.0}

  current <- readRef state
  let rotated = map (rotateX current.deltaY) <$> map (rotateY current.deltaX) <$> cube
  drawCube ctx rotated

  modifyRef state (\s -> {  dragged : s.dragged
                          , prevX   : s.prevX
                          , prevY   : s.prevY
                          , deltaX  : decelerate s.deltaX 0.99
                          , deltaY  : decelerate s.deltaY 0.99
                          })
  window >>= requestAnimationFrame (animate ctx state rotated)

drawCubeFace :: forall e. Context2D -> Array Point2D -> Eff (console :: CONSOLE, canvas :: CANVAS, dom :: DOM | e) Unit
drawCubeFace ctx face = do
    let hd = unsafePartial $ fromJust $ head face
    let tl = unsafePartial $ fromJust $ tail face

    _ <- beginPath ctx
    _ <- moveTo ctx (round hd.x) (round $ -hd.y)
    for_ tl \p -> do
      lineTo ctx (round p.x) (round $ -p.y)
    _ <- closePath ctx
    _ <- stroke ctx

    pure unit

drawCube :: forall e. Context2D -> Array (Array Point) -> Eff (console :: CONSOLE, canvas :: CANVAS, dom :: DOM | e) Unit
drawCube ctx cube = void $ do
  for_ (map orthographic <$> cube) \face ->
    drawCubeFace ctx face

initEventHandlers :: forall e. Partial => String -> Ref State -> Eff (ref :: REF, console :: CONSOLE, canvas :: CANVAS, dom :: DOM | e) Unit
initEventHandlers selector state = do
  addMouseEventHandler selector "mousedown" $ handleMouseDown state
  addMouseEventHandler selector "mousemove" $ handleMouseMove state
  addMouseEventHandler selector "mouseup" $ handleMouseUp state
  addMouseEventHandler selector "click" $ handleClick state

  addTouchEventHandler selector "touchstart" $ handleTouchStart state
  addTouchEventHandler selector "touchmove" $ handleTouchMove state
  addTouchEventHandler selector "touchend" $ handleTouchEnd state


main :: forall e. Partial => Eff (ref :: REF, console :: CONSOLE, canvas :: CANVAS, dom :: DOM | e) Unit
main = void do
  state <- newRef {  dragged : false
                  , prevX : 0
                  , prevY : 0
                  , deltaX : 0.0
                  , deltaY : 0.0
                  }
  let selector = "canvas"

  mCanvas <- getCanvasElementById selector
  case mCanvas of
    Just canvas -> do
      ctx <- getContext2D canvas
      _ <- translate { translateX: 300.0, translateY: 200.0} ctx
      _ <- setStrokeStyle "#ff0000" ctx
      _ <- setLineWidth 1.0 ctx
      initEventHandlers selector state
      animate ctx state $ tiltedCube (-0.17) (0.17) 100.0
    Nothing -> pure unit
