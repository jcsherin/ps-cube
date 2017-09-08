module Main where

import Prelude

import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (requestAnimationFrame)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (CANVAS, Context2D, beginPath, closePath, getCanvasElementById, getContext2D, lineTo, moveTo, setStrokeStyle, stroke)

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
