module Game where

import Data.Array

data Player = PlayerX | PlayerO deriving (Eq, Show)
type Cell = Maybe Player
data State = Running | GameOver (Maybe Player) deriving (Eq, Show)

type Board = Array (Int, Int) Cell

data Game = Game { gameBoard :: Board
				 , bigBoard :: Board
				 , prevMove :: (Int,Int)
				 , currentBox :: (Int, Int)
                 , gamePlayer :: Player
                 , gameState :: State
                 } deriving (Eq, Show)


screenWidth :: Int
screenWidth = 540

screenHeight :: Int
screenHeight = 540

cellWidth :: Float
cellWidth = 60

cellHeight :: Float
cellHeight = 60

initialGame = Game { gameBoard = array indexRange $ zip (range indexRange) (repeat Nothing)
				   , bigBoard = array iRange $ zip (range iRange) (repeat Nothing)
				   , prevMove = (-1,-1)
				   , currentBox = (-1,-1)
                   , gamePlayer = PlayerO
                   , gameState = Running
                   }
    where indexRange = ((0, 0), (8, 8))
    	  iRange = ((0,0),(2,2))