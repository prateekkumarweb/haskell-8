module Logic where

import Data.Array
import Data.Foldable ( asum )

import Game
import Graphics.Gloss.Interface.Pure.Game

temp1 :: [(Int,Int)]
temp1 = [(0,0)]
temp2 :: [(Int,Int)]
temp2 = [(0,0)]
replace :: Int -> (Int,Int) ->[(Int,Int)] -> [(Int,Int)]
replace n newVal [] = [newVal]
replace n newVal (x:xs)
	|n == 0 = newVal:xs
	| otherwise = x:replace (n-1) newVal xs

transformGame (EventKey (SpecialKey KeyUp) Up _ _ ) game =
		if (die game) == 6 then
			if fst ((gameBoard game)!!0) == -1 then
        let temp1 = (gameBoard game)
            temp2 = (humanPieces game)
        in
				    game { gameBoard = replace 0 (-1,0) temp1 , humanPieces = [(400,500),(100,200),(300,40),(88,22)]  }
			else
				game
		else
			game

transformGame (EventKey (SpecialKey KeySpace) Up _ _ ) game =
   case gameState game of
       Running -> game { die = 6,humanPieces = [(100,250),(340,200),(300,40),(88,22)] }
       GameOver _ -> initialGame


transformGame _ game = game
					--,humanPieces = replace 0 (560,260) humanPieces
