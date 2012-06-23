-- | Usage: http-bench <total> <concurrent>
module Main where

import Control.Concurrent
import Control.Monad

import Network.HTTP

import System.Exit
import System.Environment

main = do
  args <- getArgs
  when (length args /= 3) (fail usage)
  let [url, totalStr, concurrentStr] = args
  total <- readIO totalStr
  concurrent <- readIO concurrentStr
  queue <- newChan
  count <- newMVar 0
  forM_ [1 .. concurrent] $ \_ -> forkIO $ go count queue
  forM_ [1 .. total] $ \_ -> writeChan queue $ do
    simpleHTTP (getRequest url)
    return ()
  forever $ do
    n <- readMVar count
    when (n == total) exitSuccess
    threadDelay 100

go :: MVar Int -> Chan (IO ()) -> IO ()
go count queue = forever $ do
  join (readChan queue)
  modifyMVar_ count (return . succ)

usage = "Usage: http-bench <url> <total> <concurrent>\n\
        \    Benchmark a website by requesting a URL many times concurrently\n"

