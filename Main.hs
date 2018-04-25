
module Main where

import Data.Array
import Graphics.Gloss
import Graphics.Gloss.Data.Color

import Rendering
import Game
import Logic

window = InWindow "Functional" (screenWidth, screenHeight) (100, 100)
backgroundColor = makeColor 255 255 255 128

main :: IO ()
main = play window backgroundColor 30 initialGame gameAsPicture transformGame (const id)
