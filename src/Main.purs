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

type State = {  dragged :: Boolean
              , prevX :: Int
              , prevY :: Int
              , deltaX :: Number
              , deltaY :: Number
              }

decelerate :: Number -> Number -> Number
decelerate rad factor = if (abs rad) > 0.009
  then rad * factor
  else 0.0

animate :: forall e. Context2D -> Ref State -> Array (Array Point) -> Eff (ref :: REF, console :: CONSOLE, canvas :: CANVAS, dom :: DOM | e) Unit
animate ctx state cube = void do
  _ <- clearRect ctx { x: -300.0, y: -200.0, w: 600.0, h: 400.0}

  current <- readRef state
  let rotated = map (rotateX current.deltaY) <$> map (rotateY current.deltaX) <$> cube
  drawCube ctx rotated

  modifyRef state (\s -> {  dragged : s.dragged
                          , prevX   : s.prevX
                          , prevY   : s.prevY
                          , deltaX  : decelerate s.deltaX 0.95
                          , deltaY  : decelerate s.deltaY 0.95
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

lock :: forall e. Ref State -> MouseEvent -> Eff (ref :: REF, console :: CONSOLE | e) Unit
lock state e = do
  current <- readRef state
  modifyRef state (\s -> {  dragged : true
                          , prevX   : clientX e
                          , prevY   : clientY e
                          , deltaX  : s.deltaX
                          , deltaY  : s.deltaY
                          })

rotate :: forall e. Ref State -> MouseEvent -> Eff (ref :: REF, console :: CONSOLE | e) Unit
rotate state e = do
  current <- readRef state
  if current.dragged == true
    then do
      modifyRef state (\s -> {  dragged : s.dragged
                              , prevX   : clientX e
                              , prevY   : clientY e
                              , deltaX  : toNumber (clientX e - s.prevX) * pi / 180.0
                              , deltaY  : toNumber (- (clientY e - s.prevY)) * pi / 180.0
                              })
    else pure unit

release :: forall e. Ref State -> MouseEvent -> Eff (ref :: REF, console :: CONSOLE | e) Unit
release state e = do
  current <- readRef state
  modifyRef state (\s -> {  dragged : false
                          , prevX   : s.prevX
                          , prevY   : s.prevY
                          , deltaX  : s.deltaX
                          , deltaY  : s.deltaY
                          })

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

  state <- newRef {  dragged : false
                  , prevX : 0
                  , prevY : 0
                  , deltaX : 0.0
                  , deltaY : 0.0 }

  addEventHandler "mousedown" $ lock state
  addEventHandler "mousemove" $ rotate state
  addEventHandler "mouseup" $ release state

  let cube = (map $ rotateX (-0.17)) <$> (map $ rotateY 0.17) <$> faces 100.0
  animate ctx state cube
