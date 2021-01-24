{-# LANGUAGE OverloadedStrings #-}

-- #!/usr/bin/env stack
-- stack script --resolver lts-16.11

module Main where

import Control.Exception
import Control.Monad.IO.Class (liftIO)
import Database.MongoDB

localDb :: Database
localDb = "local"

opLogColl :: Collection
opLogColl = "oplog.rs"

tailOpLog :: Pipe -> ([Document] -> IO ()) -> IO ()
tailOpLog pipe f = bracket acquire release (run . loop)
  where
    acquire = run . find $ (select [] opLogColl) {options = [TailableCursor, AwaitData, NoCursorTimeout]}
    release x = do
      print $ "mongodb: " ++ "Closing opLog cursor..."
      run $ closeCursor x
    loop cr = do
      xs <- nextBatch cr
      liftIO $
        if null xs
          then return ()
          else do
            print $ "mongodb: " ++ "nextBatch.length=" ++ show (length xs)
            f xs
      loop cr
    run = access pipe master localDb

main :: IO ()
main = do
  print $ "main" ++ "Starting..."
  bracket (connect (host "127.0.0.1")) close $ \pipe -> tailOpLog pipe print

-- >>> unwords example
example :: [String]
example = ["this", "is"]