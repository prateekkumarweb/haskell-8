
module Main where

import Data.Array
import Graphics.Gloss
import Graphics.Gloss.Data.Color

import Rendering


window = InWindow "Functional" (screenWidth, screenHeight) (100, 100)
backgroundColor = makeColor 255 255 255 128

initialGame = blank


transformGame _ game = game




main :: IO ()
main = play window backgroundColor 30 initialGame gameAsPicture transformGame (const id)
