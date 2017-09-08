module Geometry.Point where

import Prelude

import Data.Number.Format (toString)

type Point = { x :: Number
             , y :: Number
             , z :: Number
             }

type Point2D = { x :: Number
               , y :: Number
               }

point :: Number -> Number -> Number -> Point
point x y z = { x, y, z }

showPoint :: Point -> String
showPoint p = "(" <>
  toString p.x <> ", " <>
  toString p.y <> ", " <>
  toString p.z <> ")"

point2D :: Number -> Number -> Point2D
point2D x y = { x, y }

showPoint2D :: Point2D -> String
showPoint2D p = "(" <>
  toString p.x <> ", " <>
  toString p.y <> ")"

orthographic :: Point -> Point2D
orthographic p = point2D p.x p.y
