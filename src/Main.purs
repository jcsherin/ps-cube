module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (requestAnimationFrame)
import Data.Array (head, tail)
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromJust)
import Geometry.Cube (faces)
import Geometry.Point (orthographic, showPoint2D)
import Graphics.Canvas (CANVAS, Context2D, beginPath, clearRect, closePath, getCanvasElementById, getContext2D, lineTo, moveTo, setStrokeStyle, stroke, translate)
import Partial.Unsafe (unsafePartial)

animate :: forall e. Context2D -> Eff (console :: CONSOLE, canvas :: CANVAS, dom :: DOM | e) Unit
animate ctx = void do
  drawCube ctx
  window >>= requestAnimationFrame (animate ctx)

drawCube :: forall e. Context2D -> Eff (console :: CONSOLE, canvas :: CANVAS, dom :: DOM | e) Unit
drawCube ctx = void $ do
  let projected = map orthographic <$> faces 150.0
  for_ projected \face -> do

    let hd = unsafePartial $ fromJust $ head face
    _ <- moveTo ctx hd.x hd.y

    let tl = unsafePartial $ fromJust $ tail face
    for_ tl \p -> do
      lineTo ctx p.x p.y

    _ <- closePath ctx
    stroke ctx

drawTestLine :: forall e. Context2D -> Eff (dom :: DOM, canvas :: CANVAS | e) Unit
drawTestLine ctx = void do
  _ <- setStrokeStyle "#ff0000" ctx >>= beginPath
  _ <- beginPath ctx
  _ <- moveTo ctx (toNumber 10) (toNumber 10)
  _ <- lineTo ctx (toNumber 200) (toNumber 200)
  _ <- closePath ctx
  stroke ctx

main :: forall e. Partial => Eff (console :: CONSOLE, canvas :: CANVAS, dom :: DOM | e) Unit
main = void do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas
  _ <- clearRect ctx { x: 0.0, y: 0.0, w: 600.0, h: 400.0}
  _ <- translate { translateX: 300.0, translateY: 200.0} ctx
  _ <- setStrokeStyle "rgba(0, 0, 0, 0.3)" ctx

  animate ctx
