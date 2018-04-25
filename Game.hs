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
                }

cellWidth :: Int
cellWidth = 40

cellHeight :: Int
cellHeight = 40


getHomeCoords :: Player -> Int -> (Int,Int)
getHomeCoords player number =
      case player of
        Human -> case number of
                      1 -> (440,160)
                      2 -> (520,160)
                      3 -> (440,80)
                      4 -> (520,80)
        Computer -> case number of
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
                  }
