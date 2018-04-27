module Logic where

import Data.Array
import Data.Foldable ( asum )

import Game
import Graphics.Gloss.Interface.Pure.Game

isCoordCorrect = inRange ((0, 0), (8, 8))

isBoxCorrect :: Game -> (Int,Int) -> Bool
isBoxCorrect game cellCoord =
  if fst (prevMove game) == -1 && snd (prevMove game) == -1 then True
  else if fst cellCoord >= 3 * (mod (fst (prevMove game)) 3) && fst cellCoord < 3*(1 + (mod (fst (prevMove game)) 3))
        && snd cellCoord >= 3*(mod (snd (prevMove game)) 3) && snd cellCoord < 3*(1 + (mod (snd (prevMove game)) 3))
        then True
  else False

whichBox :: (Int,Int) -> (Int, Int)
whichBox cellCoord = (mod (fst cellCoord) 3, mod (snd cellCoord) 3)

switchPlayer game =
    case gamePlayer game of
      PlayerX -> game { gamePlayer = PlayerO }
      PlayerO -> game { gamePlayer = PlayerX }

checkBoxWon

playerTurn :: Game -> (Int, Int) -> Game
playerTurn game cellCoord
    | isCoordCorrect cellCoord && isBoxCorrect game cellCoord && board ! cellCoord == Nothing =
        checkGameOver
        $ checkBoxWon
        $ switchPlayer 
        $ game { gameBoard = board // [(cellCoord, Just player)]
               , currentBox =  whichBox cellCoord
               , prevMove = cellCoord
               }
    | otherwise = game
    where board = gameBoard game
          player = gamePlayer game

mousePosAsCellCoord :: (Float, Float) -> (Int, Int)
mousePosAsCellCoord (x, y) = ( floor ((y + (540 * 0.5)) / 60)
                             , floor ((x + (540 * 0.5)) / 60)
                             )

transformGame (EventKey (MouseButton LeftButton) Up _ mousePos) game =
    case gameState game of
      Running -> playerTurn game $ mousePosAsCellCoord mousePos
      GameOver _ -> initialGame
transformGame _ game = game