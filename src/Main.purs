module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Ref (REF, Ref, newRef, readRef)
import DOM (DOM)
import DOM.Event.MouseEvent (MouseEvent, clientX, clientY)
import DOM.Event.TouchEvent as T
import DOM.HTML (window)
import DOM.HTML.Window (requestAnimationFrame)
import Data.Array (head, tail)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromJust)
import Events.Mouse (addMouseEventHandler)
import Events.Touch (addTouchEventHandler)
import Geometry.Cube (rotateX, rotateY, tiltedCube)
import Geometry.Point (Point, Point2D, orthographic)
import Graphics.Canvas (CANVAS, Context2D, beginPath, clearRect, closePath, getCanvasElementById, getCanvasHeight, getCanvasWidth, getContext2D, lineTo, moveTo, setLineWidth, setStrokeStyle, stroke, translate)
import Math (round)
import Model (State, updateState, lock, rotate, release, friction)
import Partial.Unsafe (unsafePartial)

animate :: forall e. Context2D -> Number -> Number -> Ref State -> Array (Array Point) -> Eff (ref :: REF, console :: CONSOLE, canvas :: CANVAS, dom :: DOM | e) Unit
animate ctx w h state cube = void do
  _ <- clearRect ctx { x: -(w / 2.0), y: -(h / 2.0), w: w, h: h}

  current <- readRef state
  -- log $ show current.dragged <> " . " <> show current.deltaX  <> " . " <> show (-current.deltaY)
  let rotated = map (rotateX (-current.deltaY)) <$> map (rotateY current.deltaX) <$> cube
  drawCube ctx rotated

  updateState (friction 0.90) state
  window >>= requestAnimationFrame (animate ctx w h state rotated)

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

handleMouseDown :: forall e. Ref State -> MouseEvent -> Eff (ref :: REF, console :: CONSOLE | e) Unit
handleMouseDown state e = updateState (lock {x : clientX e, y: clientY e}) state

handleMouseMove :: forall e. Ref State -> MouseEvent -> Eff (ref :: REF, console :: CONSOLE | e) Unit
handleMouseMove state e = updateState (rotate {x : clientX e, y: clientY e}) state

handleMouseUp :: forall e. Ref State -> MouseEvent -> Eff (ref :: REF, console :: CONSOLE | e) Unit
handleMouseUp state e = updateState (release {x : clientX e, y: clientY e}) state

-- handleClick :: forall e. Ref State -> MouseEvent -> Eff (ref :: REF, console :: CONSOLE | e) Unit
-- handleClick state e = updateState (friction 0.1) state

handleTouchStart :: forall e. Ref State -> T.Touch -> Eff (ref :: REF, console :: CONSOLE | e) Unit
handleTouchStart state e = updateState (lock {x : T.clientX e, y: T.clientY e}) state

handleTouchMove :: forall e. Ref State -> T.Touch -> Eff (ref :: REF, console :: CONSOLE | e) Unit
handleTouchMove state e = updateState (rotate {x : T.clientX e, y: T.clientY e}) state

handleTouchEnd :: forall e. Ref State -> T.Touch -> Eff (ref :: REF, console :: CONSOLE | e) Unit
handleTouchEnd state e = updateState (release {x : T.clientX e, y: T.clientY e}) state

initEventHandlers :: forall e. Partial => String -> Ref State -> Eff (ref :: REF, console :: CONSOLE, canvas :: CANVAS, dom :: DOM | e) Unit
initEventHandlers selector state = do
  addMouseEventHandler selector "mousedown" $ handleMouseDown state
  addMouseEventHandler selector "mousemove" $ handleMouseMove state
  addMouseEventHandler selector "mouseup" $ handleMouseUp state
  -- addMouseEventHandler selector "click" $ handleClick state

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
      w <- getCanvasWidth canvas
      h <- getCanvasHeight canvas
      _ <- translate { translateX: w / 2.0, translateY: h / 2.0} ctx
      _ <- setStrokeStyle "#ff0000" ctx
      _ <- setLineWidth 1.0 ctx
      initEventHandlers selector state
      animate ctx w h state $ tiltedCube (-0.17) (0.17) 100.0
    Nothing -> pure unit
