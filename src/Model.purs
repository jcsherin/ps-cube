module Model where

import Prelude
import Math (abs, pi)
import Data.Int (toNumber)

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
