import Control.Concurrent
import Control.Concurrent.Async
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

data Future a = Return a | Future (IO a)

future :: IO a -> IO (Future a)
future action = do
    mvar <- newEmptyMVar
    _ <- async (action >>= putMVar mvar)
    return (Future (readMVar mvar))

runFuture :: Future a -> IO a
runFuture (Return a)      = return a
runFuture (Future action) = action

instance Functor Future where
    fmap f (Return a)      = Return (f a)
    fmap f (Future action) = Future (f <$> action)

instance Applicative Future where
    pure                     = Return
    (<*>) (Return f)      fu = f <$> fu
    (<*>) (Future action) fu = Future (action >>= flip liftM (runFuture fu))

instance Monad Future where
    return = Return
    (>>=) (Return a) f = f a
    (>>=) (Future a) f = Future (a >>= runFuture . f)

instance MonadIO Future where
    liftIO = Future

action1 :: IO (Future String)
action1 = future $ threadDelay 1000000 >> return "action1"

action2 :: IO (Future String)
action2 = future $ threadDelay 1000000 >> return "action2"

action3 :: IO (Future String)
action3 = future $ threadDelay 10000000 >> return "action3"

main :: IO ()
main = do
    foo <- action1
    bar <- action2

    let comb = do
                  a <- foo
                  b <- bar
                  return $ a ++ b

    c <- runFuture comb
    print c

    let comb2 = (++) <$> foo <*> bar

    c2 <- runFuture comb2
    print c2

    let comb3 = do
                   a <- foo
                   b <- bar

                   liftIO $ print "foo"
                   d <- liftIO action3

                   -- d is already running here
                   liftIO $ print "foo"
                   -- this waits until action3 is again finished
                   f <- join $ liftIO action3
                   -- this waits until d (first action3) is finished
                   e <- d

                   return $ a ++ b ++ e ++ f

    c3 <- runFuture comb3
    print c3
