module Logic where

import Data.Array
import Data.Foldable ( asum )

import Game
import Graphics.Gloss.Interface.Pure.Game

replace :: Int -> (Int,Int) ->[(Int,Int)] -> [(Int,Int)]
replace n newVal [] = [newVal]
replace n newVal (x:xs)
	|n == 0 = newVal:xs
	| otherwise = x:replace (n-1) newVal xs

transformGame (EventKey (SpecialKey KeySpace) Up _ _ ) game =
    case gameState game of
      Running -> game { die = 6 }
      GameOver _ -> initialGame
transformGame (EventKey (SpecialKey KeyPad1) Up _ _ ) game = 
		if (die game) == 6 then 
			if fst ((gameBoard game)!!0) == -1 then
				game { gameBoard = replace 0 (9,0) (gameBoard game) , humanPieces = replace 0 (560,260) (humanPieces game)  }
			else
				game
		else 
			game

transformGame _ game = game
					--,humanPieces = replace 0 (560,260) humanPieces 