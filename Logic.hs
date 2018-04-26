module Logic where

import Data.Array
import Data.Foldable ( asum )

import Game
import Graphics.Gloss.Interface.Pure.Game

getCoords1:: Int -> Int -> (Int,Int)
getCoords1 i j = if i == 7 then (580,300)
								 else if i > 7 then (380 + (j-1)*40,260)
								 else (380 + (j-1)*40,340)

getCoords2:: Int -> Int -> (Int,Int)
getCoords2 i j = if i == 7 then (300,20)
								 else if i > 7 then (260,20 + (6-j)*40)
								 else (340,20 + (6-j)*40)

getCoords3:: Int -> Int -> (Int,Int)
getCoords3 i j = if i == 7 then (20,300)
								 else if i > 7 then (20 + (6-j)*40,340)
								 else (20 + (6-j)*40,260)

getCoords4:: Int -> Int -> (Int,Int)
getCoords4 i j = if i == 7 then (300,580)
								 else if i > 7 then (340,380 + (j-1)*40)
								 else (260,380 + (j-1)*40)

getCoords :: Int -> (Int,Int)
getCoords 13 = (380,260)
getCoords 26 = (260,220)
getCoords 39 = (220,340)
getCoords 52 = (340,380)

getCoords a
	| a <= 13 = if (mod a 13) < 7 then getCoords1 (mod a 13) (mod a 7)
							else getCoords1 (mod a 13) (7- (mod a 7))
	| a <= 26 = if (mod a 13) < 7 then getCoords2 (mod a 13) (1 + (mod a 7))
							else getCoords2 (mod a 13) (6- (mod a 7))
	| a <= 39 = if (mod a 13) < 7 then getCoords3 (mod a 13) (2+ (mod a 7))
							else getCoords3 (mod a 13) (5- (mod a 7))
	| otherwise = if (mod a 13) < 7 then getCoords4 (mod a 13) (3 + (mod a 7))
							else getCoords4 (mod a 13) (4- (mod a 7))

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
