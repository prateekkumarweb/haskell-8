module Game where

import Data.Array


data Player = PlayerX | PlayerO deriving (Eq, Show)
type Cell = Maybe Player
data State = Running | GameOver (Maybe Player) deriving (Eq, Show)

type Board = Array (Int, Int) Cell

data Game = Game { gameBoard :: Board
                 , gamePlayer :: Player
                 , gameState :: State
                 } deriving (Eq, Show)


screenWidth :: Int
screenWidth = 900

screenHeight :: Int
screenHeight = 900

cellWidth :: Float
cellWidth = 100

cellHeight :: Float
cellHeight = 100

initialGame = Game { gameBoard = array indexRange $ zip (range indexRange) (repeat Nothing)
                   , gamePlayer = PlayerX
                   , gameState = Running
                   }
    where indexRange = ((0, 0), (8, 8))