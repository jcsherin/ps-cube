module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, newRef, readRef)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.Event.EventTarget (EventListener, addEventListener, eventListener)
import DOM.Event.MouseEvent (MouseEvent, clientX, clientY, eventToMouseEvent)
import DOM.Event.TouchEvent as T
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

handleMouseDown :: forall e. Ref State -> MouseEvent -> Eff (ref :: REF, console :: CONSOLE | e) Unit
handleMouseDown state e = do
  current <- readRef state
  modifyRef state (\s -> lock s {x : clientX e, y: clientY e})

handleMouseMove :: forall e. Ref State -> MouseEvent -> Eff (ref :: REF, console :: CONSOLE | e) Unit
handleMouseMove state e = do
  current <- readRef state
  modifyRef state (\s -> rotate s {x : clientX e, y: clientY e})

handleMouseUp :: forall e. Ref State -> MouseEvent -> Eff (ref :: REF, console :: CONSOLE | e) Unit
handleMouseUp state e = do
  current <- readRef state
  modifyRef state (\s -> release s {x : clientX e, y: clientY e})

handleTouchStart :: forall e. Ref State -> T.Touch -> Eff (ref :: REF, console :: CONSOLE | e) Unit
handleTouchStart state e = do
  current <- readRef state
  modifyRef state (\s -> lock s {x : T.clientX e, y: T.clientY e})

handleTouchMove :: forall e. Ref State -> T.Touch -> Eff (ref :: REF, console :: CONSOLE | e) Unit
handleTouchMove state e = do
  current <- readRef state
  modifyRef state (\s -> rotate s {x : T.clientX e, y: T.clientY e})

handleTouchEnd :: forall e. Ref State -> T.Touch -> Eff (ref :: REF, console :: CONSOLE | e) Unit
handleTouchEnd state e = do
  current <- readRef state
  modifyRef state (\s -> release s {x : T.clientX e, y: T.clientY e})

handleClick :: forall e. Ref State -> MouseEvent -> Eff (ref :: REF, console :: CONSOLE | e) Unit
handleClick state e = do
  current <- readRef state
  modifyRef state (\s -> friction s {x : clientX e, y: clientY e})

friction :: State -> { x :: Int, y :: Int } -> State
friction state _ = do
  if state.dragged == false
    then do
      { dragged : state.dragged
      , prevX   : state.prevX
      , prevY   : state.prevY
      , deltaX  : decelerate state.deltaX 0.1
      , deltaY  : decelerate state.deltaY 0.1
      }
    else state

lock :: State -> { x :: Int, y :: Int } -> State
lock state {x, y} = { dragged : true
                    , prevX : x
                    , prevY: y
                    , deltaX: state.deltaX
                    , deltaY: state.deltaY
                    }

rotate :: State -> {x :: Int, y :: Int } -> State
rotate state {x, y} = do
  if state.dragged == true
    then do
      { dragged : state.dragged
      , prevX   : x
      , prevY   : y
      , deltaX  : toNumber (x - state.prevX) * pi / 180.0
      , deltaY  : toNumber (- (y - state.prevY)) * pi / 180.0
      }
    else state

release :: State -> {x :: Int, y :: Int } -> State
release state {x, y} = do
  { dragged : false
  , prevX   : state.prevX
  , prevY   : state.prevY
  , deltaX  : state.deltaX
  , deltaY  : state.deltaY
  }

makeMouseEventHandler :: forall e. (MouseEvent -> Eff e Unit) -> EventListener e
makeMouseEventHandler on = eventListener (\ev -> do
  case (runExcept $ eventToMouseEvent ev) of
    Right e -> on e
    Left err -> pure unit
  )

addMouseEventHandler :: forall e. Partial => String -> (MouseEvent -> Eff (dom :: DOM |e) Unit) -> Eff (dom :: DOM | e) Unit
addMouseEventHandler e f = do
  doc <- window >>= document
  Just elem <- querySelector (QuerySelector "canvas") (htmlDocumentToParentNode doc)

  addEventListener (EventType e) (makeMouseEventHandler f) false (elementToEventTarget elem)

makeTouchEventHandler :: forall e. (T.Touch-> Eff e Unit) -> EventListener e
makeTouchEventHandler on = eventListener (\ev -> do
  case (runExcept $ T.eventToTouchEvent ev) of
    Right e -> do
      let touch = unsafePartial $ fromJust $ T.item 0 $ T.changedTouches e
      on touch
    Left err -> pure unit
  )

addTouchEventHandler :: forall e. Partial => String -> (T.Touch -> Eff (dom :: DOM |e) Unit) -> Eff (dom :: DOM | e) Unit
addTouchEventHandler e f = do
  doc <- window >>= document
  Just elem <- querySelector (QuerySelector "canvas") (htmlDocumentToParentNode doc)

  addEventListener (EventType e) (makeTouchEventHandler f) false (elementToEventTarget elem)

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
