module Rendering where

import Data.Array
import Graphics.Gloss

import Game

playerXColor = makeColorI 255 50 50 255
playerOColor = makeColorI 50 100 255 255
tieColor = blue
boardGridColor = makeColorI 255 255 255 255

boardAsRunningPicture board =
    pictures [
             color playerXColor $ xCellsOfBoard board,
             color playerOColor $ oCellsOfBoard board,
             color boardGridColor $ boardGrid
             ]

outcomeColor (Just PlayerX) = playerXColor
outcomeColor (Just PlayerO) = playerOColor
outcomeColor Nothing = tieColor

snapPictureToCell picture (row, column) = translate x y picture
    where x = fromIntegral column * cellWidth + cellWidth * 0.5
          y = fromIntegral row * cellHeight + cellHeight * 0.5


xCell :: Picture
xCell = pictures
        [ 
          rotate 45.0 $ rectangleSolid 75.0 10.0,
          rotate (-45.0) $ rectangleSolid 75.0 10.0
        ]

oCell :: Picture
oCell = thickCircle 25.0 10.0

cellsOfBoard :: Board -> Cell -> Picture -> Picture
cellsOfBoard board cell cellPicture =
    pictures
    $ map (snapPictureToCell cellPicture . fst)
    $ filter (\(_, e) -> e == cell)
    $ assocs board

xCellsOfBoard :: Board -> Picture
xCellsOfBoard board = cellsOfBoard board (Just PlayerX) xCell

oCellsOfBoard :: Board -> Picture
oCellsOfBoard board = cellsOfBoard board (Just PlayerO) oCell

horizontalLines :: Picture
horizontalLines =
  pictures
  $ concatMap (\i -> [line [(0,100*i),(900,100*i)]])
    [0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0]

verticalLines :: Picture
verticalLines =
  pictures
  $ concatMap (\i -> [line [(100*i,0),(100*i,900)]])
    [0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0]


boardGrid :: Picture
boardGrid =
    pictures
    [
        horizontalLines,
        verticalLines
    ]

gameAsPicture:: Game->Picture
gameAsPicture p = translate (fromIntegral screenWidth * (-0.5))
                           (fromIntegral screenHeight * (-0.5))
                           frame
     where frame = boardGrid

transformGame _ game = game

