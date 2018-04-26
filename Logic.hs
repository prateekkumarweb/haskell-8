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
	| a <= 26 = if (mod a 13) < 7 then getCoords2 (mod a 13) (mod (1 + (mod a 7)) 7)
							else getCoords2 (mod a 13) (7- (mod a 20))
	| a <= 39 = if (mod a 13) < 7 then getCoords3 (mod a 13) (mod (2+ (mod a 7)) 7)
							else getCoords3 (mod a 13) (7- (mod a 33))
	| otherwise = if (mod a 13) < 7 then getCoords4 (mod a 13) (mod (3 + (mod a 7)) 7)
							else getCoords4 (mod a 13) (7- (mod a 46))

temp1 :: [(Int,Int)]
temp1 = [(0,0)]
temp2 :: [(Int,Int)]
temp2 = [(0,0)]
replace :: Int -> (Int,Int) ->[(Int,Int)] -> [(Int,Int)]
replace n newVal [] = [newVal]
replace n newVal (x:xs)
	|n == 0 = newVal:xs
	| otherwise = x:replace (n-1) newVal xs

addntotile :: Int -> Int -> Int
addntotile n p = if (n + p <= 52) then n+p
								 else n + p -52

transformGame (EventKey (SpecialKey KeyTab) Up _ _ ) game =
		if (die game) == 6 then
				if fst ((gameBoard game)!!0) == -1 then
      	game { gameBoard = replace 0 (9,0) temp1 , humanPieces = replace 0 (getCoords 9) temp2  }
				else
					game { gameBoard = replace 0 (addntotile 6 (fst (temp1!!0)),6 + snd (temp1!!0)) temp1 , humanPieces = replace 0 (getCoords (addntotile 6 (fst (temp1!!0)))) temp2  }
		else
			game
		where
			temp1 = (gameBoard game)
			temp2 = (humanPieces game)

transformGame (EventKey (SpecialKey KeySpace) Up _ _ ) game =
   case gameState game of
       Running -> game { die = 6 }
       GameOver _ -> initialGame


transformGame _ game = game
					--,humanPieces = replace 0 (560,260) humanPieces
