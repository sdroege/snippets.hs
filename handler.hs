{-# LANGUAGE ExistentialQuantification #-}

import Control.Monad.State

data Handler a b = forall s. Handler s (a -> State s b)

run :: Handler a b -> a -> (b, Handler a b)
run (Handler z f) a = (b, Handler z' f)
  where (b, z') = runState (f a) z

runAll :: [Handler a b] -> a -> [(b, Handler a b)]
runAll hs s = foldr (\h r -> run h s : r) [] hs

-- A simple handler with state of type integer.
handlerA ::Handler String String
handlerA = Handler (0::Int) $ \s -> do modify (+ 1)
                                       i <- get
                                       return (s ++ " " ++ show i)

-- A simple handler with state of type String.
handlerB :: Handler String String
handlerB = Handler "" $ \s -> do modify (++ s)
                                 get

handlers :: [Handler String String]
handlers = [handlerA, handlerB]

result :: [Handler String String] -> String -> ([String], [Handler String String])
result h = unzip . runAll h

foreverHandleLine :: [Handler String String] -> IO ()
foreverHandleLine h = do
    line <- getLine
    let (output, h') = result h line
    mapM_ putStrLn output
    foreverHandleLine h'

main :: IO ()
main = foreverHandleLine handlers
