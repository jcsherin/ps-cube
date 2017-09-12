module Model where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, readRef)
import Data.Int (toNumber)
import Math (abs, pi)

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

friction :: Number -> State -> State
friction factor state = do
  if state.dragged == false
    then do
      { dragged : state.dragged
      , prevX   : state.prevX
      , prevY   : state.prevY
      , deltaX  : decelerate state.deltaX factor
      , deltaY  : decelerate state.deltaY factor
      }
    else state

lock :: { x :: Int, y :: Int } -> State -> State
lock {x, y} state = { dragged : true
                    , prevX : x
                    , prevY: y
                    , deltaX: state.deltaX
                    , deltaY: state.deltaY
                    }

rotate :: { x :: Int, y :: Int } -> State -> State
rotate {x, y} state = do
  if state.dragged == true
    then do
      { dragged : state.dragged
      , prevX   : x
      , prevY   : y
      , deltaX  : toNumber (x - state.prevX) * pi / 180.0
      , deltaY  : toNumber (y - state.prevY) * pi / 180.0
      }
    else state

release :: { x :: Int, y :: Int } -> State -> State
release {x, y} state = do
  { dragged : false
  , prevX   : state.prevX
  , prevY   : state.prevY
  , deltaX  : state.deltaX
  , deltaY  : state.deltaY
  }

updateState :: forall e. (State -> State) -> Ref State -> Eff (ref :: REF | e) Unit
updateState f state = do
  current <- readRef state
  modifyRef state (\s -> f s)
