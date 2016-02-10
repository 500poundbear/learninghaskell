module GuessGame where
import System.Random
import Control.Monad.State

takeGuess :: StateT (Int, Int, (Int, Int)) IO ()
takeGuess = do 
        (tgt, cnt, (left, right)) <- get
        lift $ putStrLn$ "Left: "++ show left ++ " right: "++show right
        let cnt' = cnt + 1
        lift $ putStrLn $ show tgt ++ "TARG"
        fat <- lift getLine
        lift $ print cnt
        let uinp = read fat
        case compare uinp tgt of
            LT -> do 
                lift $ putStrLn "too low"
                put (tgt, cnt', (uinp, right))
                takeGuess 
            GT -> do
                lift $ putStrLn "too high"
                put (tgt, cnt', (left, uinp))
                takeGuess
            EQ -> do
                lift $ putStrLn "Alright"
main = do
    ans <-getStdRandom (randomR(1::Int, 100))
    result <- runStateT (takeGuess) (ans, 0, (1, 100))
    print result

