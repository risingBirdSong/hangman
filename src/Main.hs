module Main where
import Data.List
import System.Random

type WordList = [String]
type WordToGuess = String
type GuessState = [Maybe Char]
type GuessedList = String
minWordLength = 3
maxWordLength = 9

data Puzzle = Puzzle WordToGuess GuessState GuessedList

instance Show Puzzle where
    show (Puzzle _ discovered guessed) =
      (fmap renderPuzzleChar discovered)
      ++ " Guessed so far: " ++ guessed


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

-- testPuzzle = Puzzle ""
charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle wordguess _ _ ) guess = guess `elem` wordguess  

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) guess = guess `elem` guessed 

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar (Just x) = x
renderPuzzleChar Nothing  = '_'


testPuzzle = Puzzle "theft" [Just 't',Just 'h',Nothing,Nothing,Nothing] "ath" 
-- main :: IO WordList
main = do
  allwords <- gameWords
  randomWord <- randomWord allwords 
  putStrLn randomWord
  return ()
