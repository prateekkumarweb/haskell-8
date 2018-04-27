module Logic where

import Data.Array
import Data.Foldable ( asum )
import System.Random
import System.Environment
import Control.Monad

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


tossDice :: Game -> Game
tossDice game = game{die = head (randomNumbers game), randomNumbers = temp}
									where temp = tail (randomNumbers game)



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

doDelay :: Int -> Game -> Game
doDelay 0 game = game
doDelay n game = doDelay (n-1) game

checkGameOver :: Int -> Game -> Game
checkGameOver i game =
		if (snd (board!!(i*4))) == 59 then
			if (snd (board!!(i*4 + 1))) == 59 then
				if (snd (board!!(i*4+2))) == 59 then
					if (snd (board!!(i*4+3))) == 59 then
						if i == 0 then
							game {gameState = GameOver Human}
						else
							game {gameState = GameOver Computer}
					else
						game
				else
					game
			else
				game
		else
			game
		where board = (gameBoard game)

changeTurn :: Game -> Game
changeTurn game = game{ gamePlayer = Human}

computerTurn:: Game -> Game
computerTurn game =
			changeTurn
			$ checkGameOver 1
			$ playTurn
			$ checkGameOver 0 game

playTurn:: Game -> Game
playTurn game
		| (gameState game) == Running =
				playComp
				$ tossDice
				$ game{gamePlayer = Computer}
		| otherwise = game

playComp :: Game -> Game
playComp game =
	if i > 3 && i < 8 then
		movePiece i game
	else if j > 3 && j < 8 then
		movePiece j game
	--else if k > 3 && k < 8 then
	--	movePiece k game
	else if (die game) == 6 && m > 3 && m < 8 then
		game { gameBoard = replace m (35,0) temp1 , computerPieces = replace (mod m 4) (getCoords 35) temp2}
	else
		movePiece (getPiece game) game
	where
		i = checkHome game
		j = checkKill game
		m = getInsidePiece game
		temp1 = (gameBoard game)
		temp2 = (computerPieces game)
	--k = checkSafe game

getInsidePiece:: Game -> Int
getInsidePiece game
		| (fst (board!!4)) == -1 = 4
		| (fst (board!!5)) == -1 = 5
		| (fst (board!!6)) == -1 = 6
		| (fst (board!!7)) == -1 = 7
		|otherwise = -1
		where
			board = (gameBoard game)

getPiece:: Game -> Int
getPiece game
		| (fst (board!!4)) /= -1 = 4
		| (fst (board!!5)) /= -1 = 5
		| (fst (board!!6)) /= -1 = 6
		| (fst (board!!7)) /= -1 = 7
		|otherwise = -1
		where
			board = (gameBoard game)

checkHome :: Game -> Int
checkHome game
	| d + (snd (board!!4)) > 53 = 4
	| d + (snd (board!!5)) > 53 = 5
	| d + (snd (board!!6)) > 53 = 6
	| d + (snd (board!!7)) > 53 = 7
	| otherwise = -1
	where
		d = (die game)
		board = (gameBoard game)

checkKill :: Game -> Int
checkKill game
	| addntotile d (fst (board!!4)) == (fst (board!!0)) || addntotile d (fst (board!!4)) == (fst (board!!1)) || addntotile d (fst (board!!4)) == (fst (board!!2)) || addntotile d (fst (board!!4)) == (fst (board!!3)) = 4
	| addntotile d (fst (board!!5)) == (fst (board!!0)) || addntotile d (fst (board!!5)) == (fst (board!!1)) || addntotile d (fst (board!!5)) == (fst (board!!2)) || addntotile d (fst (board!!5)) == (fst (board!!3)) = 5
	| addntotile d (fst (board!!6)) == (fst (board!!0)) || addntotile d (fst (board!!6)) == (fst (board!!1)) || addntotile d (fst (board!!6)) == (fst (board!!2)) || addntotile d (fst (board!!6)) == (fst (board!!3)) = 6
	| addntotile d (fst (board!!7)) == (fst (board!!0)) || addntotile d (fst (board!!7)) == (fst (board!!1) )|| addntotile d (fst (board!!7)) == (fst (board!!2)) || addntotile d (fst (board!!7)) == (fst (board!!3)) = 7
	| otherwise = -1
	where
		d = (die game)
		board = (gameBoard game)

{-checkSafe :: Game -> Int
checkSafe game
	| (addntotile d (fst (board!!4))) - (fst (board!!0)) <= 6 || (addntotile d (fst (board!!4))) - (fst (board!!1)) <=6 || (addntotile d (fst (board!!4))) - (fst (board!!2)) <= 6 || (addntotile d (fst (board!!4))) - (fst (board!!3)) <=6 = 4
	| (addntotile d (fst (board!!5))) - (fst (board!!0)) <= 6 || (addntotile d (fst (board!!5))) - (fst (board!!1)) <=6 || (addntotile d (fst (board!!5))) - (fst (board!!2)) <= 6 || (addntotile d (fst (board!!5))) - (fst (board!!3)) <=6 = 5
	| (addntotile d (fst (board!!6))) - (fst (board!!0)) <= 6 || (addntotile d (fst (board!!6))) - (fst (board!!1)) <=6 || (addntotile d (fst (board!!6))) - (fst (board!!2)) <= 6 || (addntotile d (fst (board!!6))) - (fst (board!!3)) <=6 = 6
	| (addntotile d (fst (board!!7))) - (fst (board!!0)) <= 6 || (addntotile d (fst (board!!7))) - (fst (board!!1)) <=6 || (addntotile d (fst (board!!7))) - (fst (board!!2)) <= 6 || (addntotile d (fst (board!!7))) - (fst (board!!3)) <=6 = 7
	| otherwise = -1
	where
		d = (die game)
		board = (gameBoard game)-}

movePiece :: Int -> Game -> Game
movePiece i game =
	if i >= 0 then
		killPieces i (game { gameBoard = replace i (addntotile (die game) (fst (temp1!!i)),(die game) + (snd (temp1!!i))) temp1 , computerPieces = replace (mod i 4) (getCoords (addntotile (die game) (fst (temp1!!i)))) temp2  })
	else
		game
	where
		temp1 = (gameBoard game)
		temp2 = (computerPieces game)

killPieces :: Int -> Game -> Game
killPieces i game = kill i 0 $ kill i 1 $ kill i 2 $ kill i 3 game

kill:: Int -> Int -> Game -> Game
kill i j game =
	if ((fst (board!!i))) == (fst (board!!j)) then
		game {gameBoard = replace j (-1,0) temp1 , humanPieces = replace j (getHomeCoords 0 (j+1)) temp2 }
	else
		game
	where
		d = (die game)
		board = (gameBoard game)
		temp1 = (gameBoard game)
		temp2 = (humanPieces game)

killCompPieces:: Int -> Game -> Game
killCompPieces i game = killComp i 0 $ killComp i 1 $ killComp i 2 $ killComp i 3 game

killComp :: Int -> Int -> Game -> Game
killComp i j game =
	if ( (fst (board!!i))) == (fst (board!!j)) then
		game {gameBoard = replace j (-1,0) temp1 , computerPieces = replace (mod j 4) (getHomeCoords 1 ((mod j 4)+1)) temp2 }
	else
		game
	where
		d = (die game)
		board = (gameBoard game)
		temp1 = (gameBoard game)
		temp2 = (computerPieces game)

transformGame (EventKey (SpecialKey KeyUp) Up _ _ ) game =
	if (gamePlayer game) == Human then
		if (die game) == 6 then
				if fst ((gameBoard game)!!0) == -1 then
      	computerTurn $ killCompPieces 0 (game { gameBoard = replace 0 (9,0) temp1 , humanPieces = replace 0 (getCoords 9) temp2  })
				else
				computerTurn $ killCompPieces 0 (game { gameBoard = replace 0 (addntotile 6 (fst (temp1!!0)),6 + snd (temp1!!0)) temp1 , humanPieces = replace 0 (getCoords (addntotile 6 (fst (temp1!!0)))) temp2  })
		else if fst ((gameBoard game)!!0) /= -1 then
			computerTurn $ killCompPieces 0  (game { gameBoard = replace 0 (addntotile (die game) (fst (temp1!!0)),(die game) + snd (temp1!!0)) temp1 , humanPieces = replace 0 (getCoords (addntotile (die game) (fst (temp1!!0)))) temp2  })
		else
			computerTurn(game)
	else
		game
		where
			temp1 = (gameBoard game)
			temp2 = (humanPieces game)

transformGame (EventKey (SpecialKey KeyDown) Up _ _ ) game =
	if (gamePlayer game) == Human then
		if (die game) == 6 then
				if fst ((gameBoard game)!!1) == -1 then
      	game { gameBoard = replace 1 (9,0) temp1 , humanPieces = replace 1 (getCoords 9) temp2  }
				else
					game { gameBoard = replace 1 (addntotile 6 (fst (temp1!!1)),6 + snd (temp1!!1)) temp1 , humanPieces = replace 1 (getCoords (addntotile 6 (fst (temp1!!1)))) temp2  }
		else if fst ((gameBoard game)!!1) /= -1 then
			game { gameBoard = replace 1 (addntotile (die game) (fst (temp1!!1)),(die game) + snd (temp1!!1)) temp1 , humanPieces = replace 1 (getCoords (addntotile (die game) (fst (temp1!!1)))) temp2  }
		else
			game
	else
		game
		where
			temp1 = (gameBoard game)
			temp2 = (humanPieces game)

transformGame (EventKey (SpecialKey KeySpace) Up _ _ ) game =
   	case gameState game of
       Running -> tossDice game
       GameOver _ -> initialGame


transformGame _ game = game
