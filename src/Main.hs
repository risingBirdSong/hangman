module Main where
import System.Random

type WordList = [String]
minWordLength = 3
maxWordLength = 9

data Puzzle = Puzzle String [Maybe Char] [Char] deriving (Show) 

freshPuzzle :: String -> Puzzle
freshPuzzle str = Puzzle str (map (\x -> Nothing) str) []  


allWords :: IO WordList
allWords = do
  dict <- readFile "words.txt"
  return (lines dict)

gameWords :: IO WordList
gameWords = do 
  aw <- allWords
  return (filter gamelength aw)
  where gamelength w = 
          let l = length (w :: String)
          in l > minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord wl = do
  randomIndex <- randomRIO ( 0 , length wl -1 )
  return $ wl !! randomIndex


-- test = ran 
-- main :: IO WordList
main = do
  allwords <- gameWords
  randomWord <- randomWord allwords 
  putStrLn randomWord
  return ()
