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
import Geometry.Cube (faces, rotateX, rotateY)
import Geometry.Point (Point, orthographic)
import Model (State, decelerate)
import Events.Mouse (addMouseEventHandler, handleClick, handleMouseDown, handleMouseMove, handleMouseUp)
import Events.Touch (addTouchEventHandler, handleTouchEnd, handleTouchMove, handleTouchStart)
import Graphics.Canvas (CANVAS, Context2D, beginPath, clearRect, closePath, getCanvasElementById, getContext2D, lineTo, moveTo, setLineWidth, setStrokeStyle, stroke, translate)
import Math (round)
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

main :: forall e. Partial => Eff (ref :: REF, console :: CONSOLE, canvas :: CANVAS, dom :: DOM | e) Unit
main = void do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  _ <- translate { translateX: 300.0, translateY: 200.0} ctx
  _ <- setStrokeStyle "#ff0000" ctx
  _ <- setLineWidth 1.0 ctx

  state <- newRef {  dragged : false
                  , prevX : 0
                  , prevY : 0
                  , deltaX : 0.0
                  , deltaY : 0.0 }

  addMouseEventHandler "mousedown" $ handleMouseDown state
  addMouseEventHandler "mousemove" $ handleMouseMove state
  addMouseEventHandler "mouseup" $ handleMouseUp state

  addTouchEventHandler "touchstart" $ handleTouchStart state
  addTouchEventHandler "touchmove" $ handleTouchMove state
  addTouchEventHandler "touchend" $ handleTouchEnd state

  addMouseEventHandler "click" $ handleClick state

  let cube = (map $ rotateX (-0.17)) <$> (map $ rotateY 0.17) <$> faces 100.0
  animate ctx state cube
