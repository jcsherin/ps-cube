module Events.Mouse where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, Ref)
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
import Model (State, friction, lock, release, rotate, updateState)

makeMouseEventHandler :: forall e. (MouseEvent -> Eff e Unit) -> EventListener e
makeMouseEventHandler on = eventListener (\ev -> do
  case (runExcept $ eventToMouseEvent ev) of
    Right e -> on e
    Left err -> pure unit
  )

addMouseEventHandler :: forall e. Partial => String -> String -> (MouseEvent -> Eff (dom :: DOM |e) Unit) -> Eff (dom :: DOM | e) Unit
addMouseEventHandler s e f = do
  doc <- window >>= document
  Just elem <- querySelector (QuerySelector s) (htmlDocumentToParentNode doc)

  addEventListener (EventType e) (makeMouseEventHandler f) false (elementToEventTarget elem)

handleMouseDown :: forall e. Ref State -> MouseEvent -> Eff (ref :: REF, console :: CONSOLE | e) Unit
handleMouseDown state e = updateState (lock {x : clientX e, y: clientY e}) state

handleMouseMove :: forall e. Ref State -> MouseEvent -> Eff (ref :: REF, console :: CONSOLE | e) Unit
handleMouseMove state e = updateState (rotate {x : clientX e, y: clientY e}) state

handleMouseUp :: forall e. Ref State -> MouseEvent -> Eff (ref :: REF, console :: CONSOLE | e) Unit
handleMouseUp state e = updateState (release {x : clientX e, y: clientY e}) state

handleClick :: forall e. Ref State -> MouseEvent -> Eff (ref :: REF, console :: CONSOLE | e) Unit
handleClick state e = updateState (friction 0.1) state
