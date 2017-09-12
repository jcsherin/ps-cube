module Events.Mouse where

import Prelude
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, readRef)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Except (runExcept)
import Data.Maybe (Maybe(..))
import DOM (DOM)
import DOM.Event.MouseEvent (MouseEvent, clientX, clientY, eventToMouseEvent)
import DOM.HTML (window)
import DOM.HTML.Window (document)
import DOM.Node.ParentNode (QuerySelector(..), querySelector)
import DOM.HTML.Types (htmlDocumentToParentNode)
import DOM.Event.EventTarget (EventListener, addEventListener, eventListener)
import DOM.Event.Types (EventType(..))
import DOM.Node.Types (elementToEventTarget)
import Data.Either (Either(..))
import Model (State, friction, lock, release, rotate)

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

handleClick :: forall e. Ref State -> MouseEvent -> Eff (ref :: REF, console :: CONSOLE | e) Unit
handleClick state e = do
  current <- readRef state
  modifyRef state (\s -> friction s {x : clientX e, y: clientY e})
