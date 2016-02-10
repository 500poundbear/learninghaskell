module GuessNum where

import System.Random
import Control.Monad.State

guessSession :: Integer -> StateT Integer IO ()
guessSession x = do
    lift $ print "Input a guess"
    guess <- lift getLine
    let g = read guess :: Integer
    modify (+1) 
    lift $ print $ "You guessed: " ++ show g
    case compare g x of
        LT -> do lift $ putStrLn "Too low"
                 guessSession x
        GT -> do lift $ putStrLn "Too high"
                 guessSession x
        EQ -> lift $ putStrLn "Nice"


main = do
    print "HI"
    --gaga <- runStateT (guessSession 5) 
    answer <- getStdRandom (randomR (1 :: Integer, 100))
--    print answer
  --  print gaga
  --
    --runStateT (guessSession answer) 0
    guesses <- execStateT (guessSession answer) 0
    print $ "You took: " ++ show guesses++" tries"
