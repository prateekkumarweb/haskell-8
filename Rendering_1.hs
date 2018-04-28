module Rendering_1 where

import Data.Array
import Data.Char

import Graphics.Gloss
import Game

-- | Stores Sceen Width
screenWidth :: Int
screenWidth = 600

-- | Stores Sceen Height
screenHeight :: Int
screenHeight = 600

-- | Create Long horizontal lines for path
hlines :: Picture
hlines =
  pictures
  $ concatMap (\i -> [line [(0,40*i),(240,40*i)], line [(360,40*i),(600,40*i)]])
    [6.0 .. 9.0]

-- | Create Long vertical lines for path
vlines :: Picture
vlines =
  pictures
  $ concatMap (\i -> [line [(40*i,0),(40*i,240)], line [(40*i,360),(40*i,600)]])
    [6.0 .. 9.0]

-- | Create small horizontal lines for path
horizontalLines :: Picture
horizontalLines =
  pictures
  $ concatMap (\i -> [line [(240,40*i),(360,40*i)]])
    [0.0,1.0,2.0,3.0,4.0,5.0,6.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0]

-- | Create small vertical lines for path
verticalLines :: Picture
verticalLines =
  pictures
  $ concatMap (\i -> [line [(40*i,240),(40*i,360)]])
    [0.0,1.0,2.0,3.0,4.0,5.0,6.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0]

-- | Put together all the lines to create tiles
tiles :: Picture
tiles =
  pictures
  [
  line [(240,240),(360,360)],
  line [(240,360),(360,240)],
  hlines,
  vlines,
  horizontalLines,
  verticalLines
  ]

-- | Whie circle
whiteCircle = color (makeColorI 255 255 255 200) $ circleSolid 20

-- | Create human piece
hpiece :: Picture
hpiece = color (makeColorI 0 0 100 200) ( circleSolid 15)

-- | Create human piece 1
hpiece1 :: Picture
hpiece1 = pictures[
                    color (makeColorI 0 0 100 200) ( circleSolid 15),
                    color (makeColorI 255 255 255 200) (translate (-6) (-8) (scale 0.175 0.175 (text "1")))
                    ]

-- | Create human piece 2                    
hpiece2 :: Picture
hpiece2 = pictures[
                    color (makeColorI 0 0 100 200) ( circleSolid 15),
                    color (makeColorI 255 255 255 200) (translate (-6) (-8) (scale 0.175 0.175 (text "2")))
                    ]

-- | Create human piece 3                    
hpiece3 :: Picture
hpiece3 = pictures[
                    color (makeColorI 0 0 100 200) ( circleSolid 15),
                    color (makeColorI 255 255 255 200) (translate (-6) (-8) (scale 0.175 0.175 (text "3")))
                    ]

-- | Create human piece 4                    
hpiece4 :: Picture
hpiece4 = pictures[
                    color (makeColorI 0 0 100 200) ( circleSolid 15),
                    color (makeColorI 255 255 255 200) (translate (-6) (-8) (scale 0.175 0.175 (text "4")))
                    ]

-- | Create computer piece
compiece :: Picture
compiece = color (makeColorI 0 100 0 200) ( circleSolid 15)

-- | Create computer piece 1
compiece1 :: Picture
compiece1 = pictures[
                    color (makeColorI 0 100 0 200) ( circleSolid 15),
                    color (makeColorI 255 255 255 200) (translate (-6) (-8) (scale 0.175 0.175 (text "1")))
                    ]

-- | Create computer piece 2 
compiece2 :: Picture
compiece2 = pictures[
                    color (makeColorI 0 100 0 200) ( circleSolid 15),
                    color (makeColorI 255 255 255 200) (translate (-6) (-8) (scale 0.175 0.175 (text "2")))
                    ]

-- | Create computer piece 3
compiece3 :: Picture
compiece3 = pictures[
                    color (makeColorI 0 100 0 200) ( circleSolid 15),
                    color (makeColorI 255 255 255 200) (translate (-6) (-8) (scale 0.175 0.175 (text "3")))
                    ]

-- | Create computer piece 4
compiece4 :: Picture
compiece4 = pictures[
                    color (makeColorI 0 100 0 200) ( circleSolid 15),
                    color (makeColorI 255 255 255 200) (translate (-6) (-8) (scale 0.175 0.175 (text "4")))
                    ]


circles1 =
  pictures
  $ concatMap (\i -> [translate (toFloat (fst (getHomeCoords 0 i))) (toFloat (snd (getHomeCoords 0 i))) whiteCircle ])
    [1,2,3,4]

circles2 =
  pictures
  $ concatMap (\i -> [translate (toFloat (fst (getHomeCoords 1 i))) (toFloat (snd (getHomeCoords 1 i))) whiteCircle ])
    [1,2,3,4]

-- | Put together all the elements of the board
boardGrid :: Picture
boardGrid =
    pictures
    [
       color (makeColorI 0 255 0 200) ( polygon [(0,600),(0,360),(240,360),(240,600)]),
       color (makeColorI 0 0 255 200) ( polygon [(360,240),(360,0),(600,0),(600,240)]),
       color (makeColorI 255 255 0 200) ( polygon [(360,600),(360,360),(600,360),(600,600)]),
       color (makeColorI 255 0 0 200) ( polygon [(0,240),(0,0),(240,0),(240,240)]),
       color (makeColorI 0 255 0 200) ( polygon [(240,360),(240,240),(300,300)]),
       color (makeColorI 255 0 0 200) ( polygon [(240,240),(300,300),(360,240)]),
       color (makeColorI 0 0 255 200) ( polygon [(300,300),(360,240),(360,360)]),
       color (makeColorI 255 255 0 200) ( polygon [(300,300),(360,360),(240,360)]),
       color (makeColorI 0 255 0 200) ( polygon [(40,280),(40,360),(80,360),(80,320),(240,320),(240,280)]),
       color (makeColorI 255 0 0 200) ( polygon [(320,40),(240,40),(240,80),(280,80),(280,240),(320,240)]),
       color (makeColorI 0 0 255 200) ( polygon [(560,320),(560,240),(520,240),(520,280),(360,280),(360,320)]),
       color (makeColorI 255 255 0 200) ( polygon [(280,560),(360,560),(360,520),(320,520),(320,360),(280,360)]),
       circles1,
       circles2,
       tiles
       --translate (420, 80) hpiece,
       --translate 80 420 compiece
      ]

-- | Function to render the human pieces at their respective positions
moveHuman :: Game -> Picture
moveHuman game = pictures[
			translate  (getX (humanPieces game) 0) (getY (humanPieces game) 0) hpiece1,
			translate  (getX (humanPieces game) 1) (getY (humanPieces game) 1) hpiece2,
			translate  (getX (humanPieces game) 2) (getY (humanPieces game) 2) hpiece3,
			translate  (getX (humanPieces game) 3) (getY (humanPieces game) 3) hpiece4
		]

-- | Function to render the computer pieces at their respective positions  
moveComp :: Game -> Picture
moveComp game = pictures[
			translate  (getX (computerPieces game) 0) (getY (computerPieces game) 0) compiece1,
			translate  (getX (computerPieces game) 1) (getY (computerPieces game) 1) compiece2,
			translate  (getX (computerPieces game) 2) (getY (computerPieces game) 2) compiece3,
			translate  (getX (computerPieces game) 3) (getY (computerPieces game) 3) compiece4
		]

-- | Function to convert integer to string
intToString :: Int -> String
intToString n = "." ++ show n ++ "."

-- | Function to display the dice
dieDisplay :: Game -> Picture
dieDisplay game =
    pictures[
              color (makeColorI 255 255 255 200) ( polygon [(450,450),(510,450),(510,510),(450,510)]),
              translate 455 460 (color (makeColorI 0 0 0 200) ( scale 0.4 0.4 (text $ intToString (die game))))
            ]

-- | Function to convert int to float
toFloat::Int->Float
toFloat a = fromIntegral a/1

-- | Function to get X value of the given piece 
getX::[(Int,Int)]->Int->Float
getX arr i = toFloat $ fst (arr!!i)

-- | Function to get Y value of the given piece
getY::[(Int,Int)]->Int->Float
getY arr i = toFloat $ snd (arr!!i)

-- | Function to render board in running state
boardAsRunningPicture game =
	pictures[
        boardGrid,
				moveHuman game,
				moveComp game,
        dieDisplay game
			]

-- | Function to render board in game over state
boardAsGameOver winner
  | winner == Human = translate 50 250 $ color (makeColorI 0 0 0 200) $ scale 0.5 0.5 $ text "Human Won!!"
  | winner == Computer = translate 50 250 $ color (makeColorI 0 0 0 200) $ scale 0.5 0.5 $ text "Computer Won!!"


gameAsPicture::Game->Picture

gameAsPicture game = translate (fromIntegral screenWidth * (-0.5))
                               (fromIntegral screenHeight * (-0.5))
                               frame
    where frame = case gameState game of
                    Running -> boardAsRunningPicture game
                    GameOver winner -> boardAsGameOver winner
