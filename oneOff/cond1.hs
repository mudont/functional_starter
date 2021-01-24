#!/usr/bin/env stack
-- stack script --resolver lts-16.11
import Conduit

main :: IO ()
main = runConduit
     $ yieldMany [1..10]
    .| iterMC print
    .| liftIO (putStrLn "I was called")
    .| sinkNull

