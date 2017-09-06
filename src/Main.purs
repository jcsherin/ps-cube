module Main where

import Prelude

import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (requestAnimationFrame)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (CANVAS, Context2D, beginPath, closePath, getCanvasElementById, getContext2D, lineTo, moveTo, setStrokeStyle, stroke)

type Point = { x :: Number
             , y :: Number
             , z :: Number
             }

point :: Number -> Number -> Number -> Point
point x y z = { x: x, y: y, z: z }

origin :: Point
origin = { x: 0.0, y: 0.0, z: 0.0 }

fbl :: Number -> Point
fbl size = point (-size) (-size) (-size)

fbr :: Number -> Point
fbr size = point size (-size) (-size)

ftl :: Number -> Point
ftl size = point (-size) size (-size)

ftr :: Number -> Point
ftr size = point size size (-size)

rbl :: Number -> Point
rbl size = point (-size) (-size) size

rbr :: Number -> Point
rbr size = point size (-size) size

rtl :: Number -> Point
rtl size = point (-size) size size

rtr :: Number -> Point
rtr size = point size size size

cubeVertices :: Number -> Array Point
cubeVertices size = [ fbl size  -- 0: front bottom left
                    , fbr size  -- 1: front bottom right
                    , rbl size  -- 2: rear bottom left
                    , rbr size  -- 3: rear bottom right
                    , ftl size  -- 4: front top left
                    , ftr size  -- 5: front top right
                    , rtl size  -- 6: rear top left
                    , rtr size  -- 7: rear top right
                    ]

cubeFaces :: Number -> Array (Array Point)
cubeFaces size = [  [ fbl size, fbr size, ftr size, ftl size ] -- 0 1 5 4
                  , [ rbl size, rbr size, rtr size, rtl size ] -- 2 3 7 6
                  , [ fbl size, fbr size, rbr size, rbl size ] -- 0 1 3 2
                  , [ ftl size, ftr size, rtr size, rtl size ] -- 4 5 7 6
                  , [ fbl size, rbl size, rtl size, ftl size ] -- 0 2 6 4
                  , [ fbr size, rbr size, rtr size, ftr size ] -- 1 3 7 5
                  ]

animate :: forall e. Context2D -> Eff (dom :: DOM, canvas :: CANVAS | e) Unit
animate ctx = void do
  drawTestLine ctx
  window >>= requestAnimationFrame (animate ctx)

drawTestLine :: forall e. Context2D -> Eff (dom :: DOM, canvas :: CANVAS | e) Unit
drawTestLine ctx = void do
  _ <- setStrokeStyle "#ff0000" ctx >>= beginPath
  _ <- beginPath ctx
  _ <- moveTo ctx (toNumber 10) (toNumber 10)
  _ <- lineTo ctx (toNumber 200) (toNumber 200)
  _ <- closePath ctx
  stroke ctx

main :: forall e. Partial => Eff (dom :: DOM, canvas :: CANVAS | e) Unit
main = void do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas
  animate ctx
