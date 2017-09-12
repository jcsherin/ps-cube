module Events.Touch where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, Ref)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (document)
import DOM.Node.ParentNode (QuerySelector(..), querySelector)
import DOM.HTML.Types (htmlDocumentToParentNode)
import DOM.Event.TouchEvent as T
import DOM.Event.EventTarget (EventListener, addEventListener, eventListener)
import DOM.Event.Types (EventType(..))
import DOM.Node.Types (elementToEventTarget)
import Data.Either (Either(..))
import Partial.Unsafe (unsafePartial)
import Data.Maybe (Maybe(..), fromJust)
import Model (State, lock, release, rotate, updateState)

makeTouchEventHandler :: forall e. (T.Touch-> Eff e Unit) -> EventListener e
makeTouchEventHandler on = eventListener (\ev -> do
  case (runExcept $ T.eventToTouchEvent ev) of
    Right e -> do
      let touch = unsafePartial $ fromJust $ T.item 0 $ T.changedTouches e
      on touch
    Left err -> pure unit
  )

addTouchEventHandler :: forall e. Partial => String -> String -> (T.Touch -> Eff (dom :: DOM |e) Unit) -> Eff (dom :: DOM | e) Unit
addTouchEventHandler s e f = do
  doc <- window >>= document
  Just elem <- querySelector (QuerySelector s) (htmlDocumentToParentNode doc)

  addEventListener (EventType e) (makeTouchEventHandler f) false (elementToEventTarget elem)

handleTouchStart :: forall e. Ref State -> T.Touch -> Eff (ref :: REF, console :: CONSOLE | e) Unit
handleTouchStart state e = updateState (lock {x : T.clientX e, y: T.clientY e}) state

handleTouchMove :: forall e. Ref State -> T.Touch -> Eff (ref :: REF, console :: CONSOLE | e) Unit
handleTouchMove state e = updateState (rotate {x : T.clientX e, y: T.clientY e}) state

handleTouchEnd :: forall e. Ref State -> T.Touch -> Eff (ref :: REF, console :: CONSOLE | e) Unit
handleTouchEnd state e = updateState (release {x : T.clientX e, y: T.clientY e}) state

