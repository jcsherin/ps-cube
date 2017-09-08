module Geometry.Point where

import Prelude

import Data.Number.Format (toString)

type Point = { x :: Number
             , y :: Number
             , z :: Number
             }

point :: Number -> Number -> Number -> Point
point x y z = { x: x, y: y, z: z }

showPoint :: Point -> String
showPoint p = "(" <>
  toString p.x <> "," <>
  toString p.y <> "," <>
  toString p.z <> ")"
