module Geometry.Cube where

import Prelude

import Geometry.Point (Point, point)
import Math (cos, sin)

-- front bottom left
fbl :: Number -> Point
fbl edge = point (-edge) (-edge) (-edge)

-- front bottom right
fbr :: Number -> Point
fbr edge = point edge (-edge) (-edge)

-- front top left
ftl :: Number -> Point
ftl edge = point (-edge) edge (-edge)

-- front top right
ftr :: Number -> Point
ftr edge = point edge edge (-edge)

-- rear bottom left
rbl :: Number -> Point
rbl edge = point (-edge) (-edge) edge

-- rear bottom right
rbr :: Number -> Point
rbr edge = point edge (-edge) edge

-- rear top left
rtl :: Number -> Point
rtl edge = point (-edge) edge edge

-- rear top right
rtr :: Number -> Point
rtr edge = point edge edge edge

makeVertex :: Number -> (Number -> Point) -> Point
makeVertex edge f = f edge

makeVertices :: forall a. Functor a => Number -> a (Number -> Point) -> a Point
makeVertices edge fs = makeVertex edge <$> fs

faces :: Number -> Array (Array Point)
faces edge = makeVertices edge <$> [ [ fbl, fbr, ftr, ftl ]
                                   , [ rbl, rbr, rtr, rtl ]
                                   , [ fbl, fbr, rbr, rbl ]
                                   , [ ftl, ftr, rtr, rtl ]
                                   , [ fbl, rbl, rtl, ftl ]
                                   , [ fbr, rbr, rtr, ftr ]
                                   ]

rotateY :: Number -> Point -> Point
rotateY theta {x, y, z} = point
  (x * cos theta - z * sin theta) y (z * cos theta + x * sin theta)

rotateX :: Number -> Point -> Point
rotateX theta {x, y, z} = point
  x (y * cos theta - z * sin theta) (z * cos theta + y * sin theta)

tiltedCube :: Number -> Number -> Number -> Array (Array Point)
tiltedCube radX radY edge = (map $ rotateX radX) <$>
  (map $ rotateY radY) <$>
  faces edge
