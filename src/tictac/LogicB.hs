module LogicB where

import Data.Array
import Data.Foldable ( asum )

import Game
import Graphics.Gloss.Interface.Pure.Game

-- | Checks if button is pressed in correct cell
isCoordCorrect = inRange ((0, 0), (8, 8))

-- | Mathematical division
fl :: Int -> Int
fl i 
  |i < 3 = 0
  | i< 6 = 1
  | otherwise = 2

-- | Checks if the player O move is correct correspoding to player X's last move and vice versa
isBoxCorrect :: Game -> (Int,Int) -> Bool
isBoxCorrect game cellCoord =
  if fst (prevMove game) == -1 && snd (prevMove game) == -1 then True
  else if ((bBoard ! ((mod (fst mv) 3),(mod (snd mv) 3))) == Just Player_X) || ((bBoard ! ((mod (fst mv) 3),(mod (snd mv) 3))) == Just Player_O) then
        if ((bBoard ! ((fl (fst cellCoord)),(fl (snd cellCoord)))) == Just Player_X) || ((bBoard ! ((fl (fst cellCoord)),(fl (snd cellCoord)))) == Just Player_O) then False 
        else True
  else 
      if ((bBoard ! ((fl (fst cellCoord)),(fl (snd cellCoord)))) == Just Player_X )|| ((bBoard ! ((fl (fst cellCoord)),(fl (snd cellCoord) ))) == Just Player_O) then False 
      else if fst cellCoord >= 3 * (mod (fst (prevMove game)) 3) && fst cellCoord < 3*(1 + (mod (fst (prevMove game)) 3))
            && snd cellCoord >= 3*(mod (snd (prevMove game)) 3) && snd cellCoord < 3*(1 + (mod (snd (prevMove game)) 3))
            then True
      else False
  where bBoard = bigBoard game
        mv = prevMove game

-- | Takes in cell coordinate and returns the cell of big board
whichBox :: (Int,Int) -> (Int, Int)
whichBox cellCoord = (mod (fst cellCoord) 3, mod (snd cellCoord) 3)


-- | Switch between Player X and Player O
switchPlayer game =
    case gamePlayer game of
      PlayerX -> game { gamePlayer = PlayerO }
      PlayerO -> game { gamePlayer = PlayerX }

-- | Utility function for checkBoxGameWon
playerConvert :: Maybe Player -> Maybe PlayerA
playerConvert Nothing = Nothing
playerConvert player = 
  if player == Just PlayerX then Just Player_X
  else if player == Just PlayerO then Just Player_O
  else Nothing

-- | Utility function for checkBoxGameWon
full :: [Cell] -> Maybe Player
full (cell@(Just player):cells) | all (== cell) cells = Just player
full _ = Nothing

-- | Utility function for checkBoxGameWon
fullA :: [CellA] -> Maybe PlayerA
fullA (cell@(Just player):cells) | all (== cell) cells = Just player
fullA _ = Nothing

-- | Returns the player who wins
winnerGame :: Game -> Maybe PlayerA
winnerGame game = asum $ map fullA $ rows ++ cols ++ diags
    where board = (bigBoard game)
          rows  = [[board ! (i,j) | i <- [0..2]] | j <- [0..2]]
          cols  = [[board ! (j,i) | i <- [0..2]] | j <- [0..2]]
          diags = [[board ! (i,i) | i <- [0..2]]
                  ,[board ! (i,j) | i <- [0..2], let j = 2-i ]]

-- | Returns the player who wins the small 3 x 3 cell
winnerBox :: Game -> (Int,Int) -> Maybe Player
winnerBox game pos = asum $ map full $ rows ++ cols ++ diags
    where board = (gameBoard game)
          rows  = [[board ! (3* (fst pos) + i,3* (snd pos) + j) | i <- [0..2]] | j <- [0..2]]
          cols  = [[board ! (3* (fst pos) + j,3* (snd pos) + i) | i <- [0..2]] | j <- [0..2]]
          diags = [[board ! (3* (fst pos) + i,3* (snd pos) + i) | i <- [0..2]]
                  ,[board ! (3* (fst pos) + i,3* (snd pos) + j) | i <- [0..2], let j = 2-i ]]

-- | Utility function to check tie condition
countCells :: Cell -> Board  -> Int
countCells cell = length . filter ((==) cell) . elems

-- | Utility function to check tie condition
countCellsA :: CellA -> BBoard  -> Int
countCellsA cell = length . filter ((==) cell) . elems

checkBoxWon :: Game -> Game
checkBoxWon game = checkBoxGameWon game (currentBox game)

-- | Main logic function to check who won the 3 x 3 cell
checkBoxGameWon :: Game -> (Int,Int) -> Game
checkBoxGameWon game pos 
    | Just p <- playerConvert $ winnerBox game pos =
        game { bigBoard = bBoard // [(pos, Just p)] }
    | countCells Nothing board  == 0 =
        game { bigBoard = bBoard // [(pos, Just Tie)] }
    | otherwise = game
    where board = gameBoard game
          bBoard = bigBoard game

-- | Main logic function to check who won the full game
checkGameOver:: Game -> Game
checkGameOver game 
    | Just p <- winnerGame game =
        game { gameState = GameOver $ Just p }
   -- | countCells Nothing board == 0 =
      --  game { gameState = GameOver Nothing }
    | otherwise = game
    where board = bigBoard game

-- | Sequence of steps to change game state and check restriction condition
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
    | otherwise = game { flag = (temp + 1) }
    where board = gameBoard game
          player = gamePlayer game
          temp = flag game

-- | Returns the coordinate of the cell where mouse is clicked
mousePosAsCellCoord :: (Float, Float) -> (Int, Int)
mousePosAsCellCoord (x, y) = ( floor ((y + (540 * 0.5)) / 60)
                             , floor ((x + (540 * 0.5)) / 60)
                             )

transformGame (EventKey (MouseButton LeftButton) Up _ mousePos) game =
    case gameState game of
      Running -> playerTurn game $ mousePosAsCellCoord mousePos
      GameOver _ -> initialGame
transformGame _ game = game