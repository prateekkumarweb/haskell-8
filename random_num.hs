
import System.Random
import Data.List

diceRoll :: [Int]-> Int
diceRoll p = (mod (p!!0) 6) + 1	

main = do
   gen <- newStdGen
   let ns = randoms gen :: [Int]
   print $ diceRoll ns
   --print $ take 1 ns

--rollDice :: IO Int
--rollDice = getStdRandom (randomR (1,6))