
module Main where

import Data.Array
import Graphics.Gloss
import Graphics.Gloss.Data.Color

screenWidth :: Int
screenWidth = 600

screenHeight :: Int
screenHeight = 600

window = InWindow "Functional" (screenWidth, screenHeight) (100, 100)
backgroundColor = makeColor 255 255 255 128

initialGame = blank

hlines :: Picture
hlines =
  pictures
  $ concatMap (\i -> [line [(0,40*i),(240,40*i)], line [(360,40*i),(600,40*i)]])
    [6.0 .. 9.0]

vlines :: Picture
vlines =
  pictures
  $ concatMap (\i -> [line [(40*i,0),(40*i,240)], line [(40*i,360),(40*i,600)]])
    [6.0 .. 9.0]

horizontalLines :: Picture
horizontalLines =
  pictures
  $ concatMap (\i -> [line [(240,40*i),(360,40*i)]])
    [0.0,1.0,2.0,3.0,4.0,5.0,6.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0]

verticalLines :: Picture
verticalLines =
  pictures
  $ concatMap (\i -> [line [(40*i,240),(40*i,360)]])
    [0.0,1.0,2.0,3.0,4.0,5.0,6.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0]

tiles :: Picture
tiles =
  pictures
  [
  line [(240,240),(360,360)],
  line [(240,360),(360,240)],
  hlines,
  vlines,
  horizontalLines,
  verticalLines
  ]
boardGrid :: Picture
boardGrid =
    pictures
    [
       color (makeColorI 0 255 0 200) ( polygon [(0,600),(0,360),(240,360),(240,600)]),
       color (makeColorI 0 0 255 200) ( polygon [(360,240),(360,0),(600,0),(600,240)]),
       color (makeColorI 255 255 0 200) ( polygon [(360,600),(360,360),(600,360),(600,600)]),
       color (makeColorI 255 0 0 200) ( polygon [(0,240),(0,0),(240,0),(240,240)]),
       color (makeColorI 0 255 0 200) ( polygon [(240,360),(240,240),(300,300)]),
       color (makeColorI 255 0 0 200) ( polygon [(240,240),(300,300),(360,240)]),
       color (makeColorI 0 0 255 200) ( polygon [(300,300),(360,240),(360,360)]),
       color (makeColorI 255 255 0 200) ( polygon [(300,300),(360,360),(240,360)]),
       color (makeColorI 0 255 0 200) ( polygon [(40,280),(40,360),(80,360),(80,320),(240,320),(240,280)]),
       color (makeColorI 255 0 0 200) ( polygon [(320,40),(240,40),(240,80),(280,80),(280,240),(320,240)]),
       color (makeColorI 0 0 255 200) ( polygon [(560,320),(560,240),(520,240),(520,280),(360,280),(360,320)]),
       color (makeColorI 255 255 0 200) ( polygon [(280,560),(360,560),(360,520),(320,520),(320,360),(280,360)]),
       tiles
      ]
gameAsPicture::Picture->Picture
gameAsPicture p = translate (fromIntegral screenWidth * (-0.5))
                               (fromIntegral screenHeight * (-0.5))
                               frame
    where frame = boardGrid

transformGame _ game = game




main :: IO ()
main = play window backgroundColor 30 initialGame gameAsPicture transformGame (const id)
