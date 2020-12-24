module Main where
import Data.List
import Debug.Trace
import System.Random

type WordList = [String]

minWordLength = 3
maxWordLength = 9


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

type WordToGuess = String
type GuessState = [Maybe Char]
type GuessedList = String
data Puzzle = Puzzle WordToGuess GuessState GuessedList
type CurGuessChar = Char 
type CorrectGuess = Char 
-- correctGuessChar :: CorrectGuess -> CurGuessChar -> Maybe Char
-- correctGuessChar guess correct  
--   | correct == guess = Just correct 
--   | otherwise = Nothing 
cgmTesta = updateState [Just 't',Just 'h',Nothing,Nothing,Just 't'] "theft" 'z'
cgmTestb = updateState [Just 't',Just 'h',Nothing,Nothing,Just 't'] "theft" 'f'
updateState :: GuessState -> WordToGuess -> CurGuessChar -> GuessState
updateState state correctWord guess = go state correctWord []
  where go states [] stateAcc = stateAcc  
        go ((Just x):sts) (l:ls) stateAcc = go sts ls (stateAcc++[Just x]) 
        go (x:states) (crct:ltrs) stateAcc 
          | crct == guess = (go (states) ltrs (stateAcc++[Just crct])) 
          | crct /= guess = (go (states) ltrs (stateAcc++[Nothing]))
updateZip guess (Just x, ltr) = Just x             
updateZip guess (Nothing, ltr) 
  | guess == ltr = Just ltr
  | otherwise = Nothing 

updateState_ state word gs = map (updateZip gs) $ zip state word 

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle wrdToGuess guessState guessed) curGs = Puzzle wrdToGuess (updateState guessState wrdToGuess curGs) (curGs:guessed)
  
fillInCharacter_ :: Puzzle -> Char -> Puzzle
fillInCharacter_ (Puzzle wrd gsSt gsd) gs = Puzzle wrd (updateState_ gsSt wrd gs) (gs:gsd)
  




testPuzzle = Puzzle "theft" [Just 't',Just 'h',Nothing,Nothing,Just 't'] "ath" 
-- main :: IO WordList
main = do
  allwords <- gameWords
  randomWord <- randomWord allwords 
  putStrLn randomWord
  return ()

