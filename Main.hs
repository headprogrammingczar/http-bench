-- | Usage: http-bench <total> <concurrent>
module Main where

import Control.Concurrent
import Control.Monad

import Network.HTTP
import Text.Printf

import System.Exit
import System.Environment
import Control.Exception as E
import Debug.Trace

data Stats = Stats {hits :: Int, prevHits :: Int} deriving (Show)

data Interval = Interval {} deriving (Show)

interval = 10 * 1000 * 1000 -- ten seconds

main = do
  args <- getArgs
  when (length args /= 3) (fail usage)
  let [url, minConcStr, maxConcStr] = args
  minConc <- readIO minConcStr :: IO Int
  maxConc <- readIO maxConcStr :: IO Int
  currentConcurrent <- newMVar minConc
  stats <- newMVar $ Stats {hits = 0, prevHits = 0}
  flip E.catch (handleInterrupts stats) $ do
    -- create initial set of threads
    threads <- forM [1 .. minConc] $ \_ -> forkIO $ go url stats
    -- spawn thread for pool control
    forkIO $ poolControl threads (go url stats) currentConcurrent
    -- main thread does stat control
    statControl minConc maxConc currentConcurrent stats

handleInterrupts stats e | e /= UserInterrupt = E.throwIO e
                         | otherwise = do s <- readMVar stats
                                          putStr "\n\n"
                                          print s
                                          error "Exiting..."

poolControl :: [ThreadId] -> IO () -> MVar Int -> IO ()
poolControl threads action currentConcurrent = do
  -- maintain a list of type [ThreadId] that represents a threadpool
  threadDelay interval
  let currentThreads = length threads
  wantedThreads <- readMVar currentConcurrent
  -- periodically spawn or kill threads as needed to keep the pool at the size specified by an mvar
  case compare wantedThreads currentThreads of
    GT -> do let newThreads = wantedThreads - currentThreads
             tids <- forM [1 .. newThreads] $ \_ -> forkIO action
             poolControl (tids ++ threads) action currentConcurrent
    LT -> do let removeThreads = currentThreads - wantedThreads
                 (remove, keep) = splitAt removeThreads threads
             forM_ remove $ \tid -> killThread tid
             poolControl keep action currentConcurrent
    EQ -> poolControl threads action currentConcurrent

statControl :: Int -> Int -> MVar Int -> MVar Stats -> IO ()
statControl minConc maxConc currentConcurrent statsRef = forever $ do
  threadDelay interval
  -- read current stats information
  stats <- readMVar statsRef
  conc <- readMVar currentConcurrent
  -- use information from stats to update concurrency level, if necessary
  -- if we end up unable to rise above the minimum concurrency level, print message
  let wanted = case (prevHits stats `compare` hits stats) of
                      EQ -> conc
                      LT -> min maxConc (conc + 1)
                      GT -> max minConc (conc - 1)
  -- if we end up stable at the maximum concurrency level, print message
  printf "Hits: %i - Concurrent: %i\n" (hits stats) wanted
  -- reset stats for current interval
  modifyMVar_ statsRef (return . reset)
  modifyMVar_ currentConcurrent (return . const wanted)
  return ()

reset :: Stats -> Stats
reset s = s {hits = 0, prevHits = hits s}

go :: String -> MVar Stats -> IO ()
go url stats = forever $ do
  result <- simpleHTTP (getRequest url)
  let success = case result of
                  (Right response) | rspCode response == (2, 0, 0) -> True
                  _ -> False
  modifyMVar_ stats $ \s -> return s {hits = hits s + 1}

usage = "\n\
        \Usage: http-bench <url> <min-concurrent> <max-concurrent>\n\
        \    Benchmark a website by requesting a URL many times concurrently.\n\
        \    http-bench will begin at the minimum number of concurrent requests,\n\
        \    and slowly scale to the speed of your webserver, or the upper\n\
        \    concurrency limit parameter.\n"

