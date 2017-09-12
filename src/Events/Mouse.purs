module Events.Mouse where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Except (runExcept)
import Data.Maybe (Maybe(..))
import DOM (DOM)
import DOM.Event.MouseEvent (MouseEvent, eventToMouseEvent)
import DOM.HTML (window)
import DOM.HTML.Window (document)
import DOM.Node.ParentNode (QuerySelector(..), querySelector)
import DOM.HTML.Types (htmlDocumentToParentNode)
import DOM.Event.EventTarget (EventListener, addEventListener, eventListener)
import DOM.Event.Types (EventType(..))
import DOM.Node.Types (elementToEventTarget)
import Data.Either (Either(..))

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

