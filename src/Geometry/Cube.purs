module Geometry.Cube where

import Prelude
import Geometry.Point (Point, point)

origin :: Point
origin = { x: 0.0, y: 0.0, z: 0.0 }

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

vertices :: Number -> Array Point
vertices edge = makeVertex edge <$> [fbl, fbr, rbl, rbr, ftl, ftr, rtl, rtr]

faces :: Number -> Array (Array Point)
faces edge = makeVertices edge <$> [ [ fbl, fbr, ftr, ftl ]
                                   , [ rbl, rbr, rtr, rtl ]
                                   , [ fbl, fbr, rbr, rbl ]
                                   , [ ftl, ftr, rtr, rtl ]
                                   , [ fbl, rbl, rtl, ftl ]
                                   , [ fbr, rbr, rtr, ftr ]
                                   ]
