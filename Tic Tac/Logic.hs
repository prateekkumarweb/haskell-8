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

full :: [Cell] -> Maybe PlayerA
full (cell@(Just player):cells) | all (== cell) cells = Just player
full _                                                = Nothing

winnerGame :: Board -> Maybe Player
winnerGame board = asum $ map full $ rows ++ cols ++ diags
    where rows  = [[board ! (i,j) | i <- [0..2]] | j <- [0..2]]
          cols  = [[board ! (j,i) | i <- [0..2]] | j <- [0..2]]
          diags = [[board ! (i,i) | i <- [0..2]]
                  ,[board ! (i,j) | i <- [0..2], let j = 2-i ]]

winnerBox :: Board -> (Int,Int) -> Maybe PlayerA
winnerBox board pos = asum $ map full $ rows ++ cols ++ diags
    where rows  = [[board ! (3* (fst pos) + i,3* (snd pos) + j) | i <- [0..2]] | j <- [0..2]]
          cols  = [[board ! (3* (fst pos) + j,3* (snd pos) + i) | i <- [0..2]] | j <- [0..2]]
          diags = [[board ! (3* (fst pos) + i,3* (snd pos) + i) | i <- [0..2]]
                  ,[board ! (3* (fst pos) + i,3* (snd pos) + j) | i <- [0..2], let j = 2-i ]]

countCells :: Cell -> Board  -> Int
countCells cell = length . filter ((==) cell) . elems

checkBoxWon :: Game -> Game
checkBoxWon game = checkGameWon game (currentBox game)

checkGameWon :: Game -> (Int,Int) -> Game
checkGameWon game pos 
    | Just p <- winnerBox board pos =
        game { bigBoard = bBoard // [(pos, Just p)] }
    | countCells Nothing board  == 0 =
        game { bigBoard = bBoard // [(pos, Tie)] }
    | otherwise = game
    where board = gameBoard game
          bBoard = bigBoard game

checkGameOver:: Game -> Game
checkGameOver game 
    | Just p <- winnerGame board =
        game { gameState = GameOver $ Just p }
    | countCells Nothing board == 0 =
        game { gameState = GameOver Nothing }
    | otherwise = game
    where board = bigBoard game

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