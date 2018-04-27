module Game where

import Data.Array

data Player = Human | Computer deriving (Eq,Show)
type Cell = Maybe Player
data State = Running | GameOver (Player) deriving (Eq, Show)

type Board =[(Int,Int)]

data Game = Game{ gameBoard :: Board
                , gamePlayer :: Player
                , gameState :: State
                , humanPieces :: [(Int,Int)]
                , computerPieces :: [(Int,Int)]
                , die :: Int
                , randomNumbers :: [Int]
                }

cellWidth :: Int
cellWidth = 40

cellHeight :: Int
cellHeight = 40


getHomeCoords :: Int -> Int -> (Int,Int)
getHomeCoords player number =
      case player of
        0 -> case number of
                      1 -> (440,160)
                      2 -> (520,160)
                      3 -> (440,80)
                      4 -> (520,80)
        1 -> case number of
                      1 -> (80,520)
                      2 -> (160,520)
                      3 -> (80,440)
                      4 -> (160,440)

initialGame = Game{ gameBoard = [(-1,0),(-1,0),(-1,0),(-1,0),(-1,0),(-1,0),(-1,0),(-1,0)]
                  , gamePlayer = Human
                  , gameState = Running
                  , humanPieces = [(440,160),(520,160),(440,80),(520,80)]
                  , computerPieces = [(80,520),(160,520),(80,440),(160,440)]
                  , die = 0
                  , randomNumbers = [1, 6, 5, 1, 6, 1, 5, 1, 6, 5, 2, 2, 2, 5, 2, 5, 4, 4, 2, 1, 4, 3, 5, 4, 6, 2, 2, 2, 2, 4, 4, 5, 6, 1, 5, 2, 6, 1, 6, 5, 2, 5, 6, 6, 5, 6, 2, 2, 6, 2, 3, 2, 2, 1, 1, 6, 5, 5, 1, 6, 4, 6, 6, 5, 6, 5, 2, 2, 4, 6, 3, 3, 5, 6, 3, 4, 1, 4, 1, 1, 6, 3, 3, 1, 6, 1, 4, 2, 4, 5, 6, 2, 4, 5, 6, 6, 4, 3, 4, 6]
                  }
