module Main where
import Control.Monad.State
import Control.Monad.State.Lazy

eg1 :: State (Integer, Integer) Integer
eg1 = do
    replicateM 10 $ do
        (n, m) <- get
        put (m, n + m)
    (n, _) <- get
    return n

eg2 :: StateT (Integer, Integer) IO Integer
eg2 = do
    replicateM 10 $ do
        (n, m) <- get
        lift $ print n
        put (m, n + m)
    (n, _) <- get
    return n

eg3 :: StateT (Integer, Char) IO Integer
eg3 = do
    replicateM 10 $ do
        (n, m) <- get
        lift $ print n
        name <- lift getLine
        put (n + 1, m)
    (n, _) <- get
    return n

code :: StateT [Integer] IO ()
code = do
    x <- pop
    io $ print x
    y <- pop

    return ()

pop :: StateT [Integer] IO Integer
pop = do
    (x:xs) <- get
    put xs
    lift $ print "HALALA"
    return x

io :: IO a -> StateT [Integer] IO a
io = liftIO


main = do
    print "HI"
--    print $ runState eg1 (0,1)
    --runStateT eg2 (0, 1)
    liftIO $ runStateT eg3 (0,'c')
    runStateT code [1..]
