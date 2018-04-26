module Rendering where

import Data.Array
import Graphics.Gloss

import Game

playerXColor = makeColorI 255 50 50 255
playerOColor = makeColorI 50 100 255 255
tieColor = makeColorI 0 0 100 255
boardGridColor = makeColorI 255 255 255 255

boardAsRunningPicture board =
    pictures [
                color playerXColor $ xCellsOfBoard board,
                color playerOColor $ oCellsOfBoard board,
                boardAsGrid
             ]

outcomeColor (Just PlayerX) = playerXColor
outcomeColor (Just PlayerO) = playerOColor
outcomeColor Nothing = tieColor

snapPictureToCell picture (row, column) = translate x y picture
    where x = fromIntegral column * 60 + 60 * 0.5
          y = fromIntegral row * 60 + 60 * 0.5


xCell :: Picture
xCell = pictures
        [ 
          rotate 45.0 $ rectangleSolid 48.0 8.0,
          rotate (-45.0) $ rectangleSolid 48.0 8.0
        ]

oCell :: Picture
oCell = thickCircle 12.0 10.0

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
  $ concatMap (\i -> [line [(0,60*i),(540,60*i)]])
    [0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0]

verticalLines :: Picture
verticalLines =
  pictures
  $ concatMap (\i -> [line [(60*i,0),(60*i,540)]])
    [0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0]

mainGrid :: Picture
mainGrid =
    pictures
    $ concatMap (\i -> [ line [ (i * 180, 0.0), (i * 180, 540)]
                       , line [ (0.0, i * 180), (540, i * 180)]
                       ])
      [0.0,1.0,2.0]

boardAsGrid :: Picture
boardAsGrid =
    pictures
    [
        color (makeColorI 175 175 175 255) horizontalLines,
        color (makeColorI 175 175 175 255) verticalLines,
        color (makeColorI 0 0 0 255) mainGrid
    ]

boardAsPicture board =
    pictures
    [
        xCellsOfBoard board
      , oCellsOfBoard board
      --, horizontalLines,
      --, verticalLines,
      , mainGrid
    ]

boardAsGameOverPicture winner board = color (outcomeColor winner) (boardAsPicture board)

gameAsPicture:: Game->Picture
gameAsPicture game = translate (fromIntegral screenWidth * (-0.5))
                           (fromIntegral screenHeight * (-0.5))
                           frame
     where frame = case gameState game of
                    Running -> boardAsRunningPicture (gameBoard game)
                    GameOver winner -> boardAsGameOverPicture winner (gameBoard game)


