{-# LANGUAGE BlockArguments #-}
module Main where
import Control.Monad
import Data.List
import Data.Char
import Debug.Trace
import System.Random
import System.Exit

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

-- updateState_ state word gs = map (updateZip gs) $ zip state word 
updateState_ state word gs = map (updateZip gs) $ zip state word 

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle wrdToGuess guessState guessed) curGs = Puzzle wrdToGuess (updateState guessState wrdToGuess curGs) (curGs:guessed)
  
fillInCharacter_ :: Puzzle -> Char -> Puzzle
fillInCharacter_ (Puzzle wrd gsSt gsd) gs = Puzzle wrd (updateState_ gsSt wrd gs) (gs:gsd)
  
handleGuess :: Puzzle -> Char -> Integer  -> IO (Puzzle, Integer )
handleGuess puzzle guess cnt = do
    -- putStrLn $ "Your guess was: " ++ [guess]
    case (charInWord puzzle guess
       , alreadyGuessed puzzle guess) of
      (_, True) -> do
        putStrLn "You already guessed that character, pick something else!"
        return (puzzle, cnt)
      (True, _) -> do
        putStrLn "good guess!"
        return (fillInCharacter puzzle guess, cnt)
      (False, _) -> do
        putStrLn "This character wasn't in the word, try again."
        return (fillInCharacter puzzle guess, succ cnt)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
  if (length guessed) > 8 then
    do  putStrLn "You lose!"
        putStrLn $ "The word was: " ++ wordToGuess
        exitSuccess
  else return ()

isJust (Just x) = True 
isJust (Nothing ) = False  


gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar then
    do putStrLn "You win!"
       exitSuccess
    else return ()

runGame :: Puzzle -> Integer -> IO ()
runGame puzzle cnt = forever $ do
  gameWin puzzle
  gameOver puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStrLn ("Guess a letter: " ++ show cnt ++ " <-count")
  guess <- getLine
  -- let test = handleGuess puzzle guess
  case guess of
    [c] ->  do
        handled <- (handleGuess puzzle c cnt) 
        runGame (fst handled)  (snd handled)
    "qqq" -> do
        putStrLn "quiting"
        exitSuccess
    _ -> putStrLn "Your guess must be a single character"
  -- case guess of
  --     [c] -> handleGuess puzzle c cnt >>= runGame cnt
  --     _ -> putStrLn "Your guess must be a single character"
        
-- two questions on this... i'd like to provide an extra argument, a number, to both handleGuess and runGame. How can I do this?
-- also, how would this be converted to do notation?
-- ```hs
--   case guess of
--       [c] -> handleGuess puzzle c >>= runGame
--       _ -> putStrLn "Your guess must be a single character"
-- ```


testPuzzle = Puzzle "theft" [Just 't',Just 'h',Nothing,Nothing,Just 't'] "ath" 
-- main :: IO WordList
main :: IO ()
main = do
  allWords <- gameWords
  word <- randomWord allWords
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle 0
