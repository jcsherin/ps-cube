module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (CANVAS, Context2D, beginPath, closePath, getCanvasElementById, getContext2D, lineTo, moveTo, setStrokeStyle, stroke)

main :: forall e. Partial => Eff (canvas :: CANVAS | e) Context2D
main = do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas
  _ <- setStrokeStyle "#ff0000" ctx
  _ <- beginPath ctx
  _ <- moveTo ctx (toNumber 10) (toNumber 10)
  _ <- lineTo ctx (toNumber 200) (toNumber 200)
  _ <- closePath ctx
  stroke ctx

