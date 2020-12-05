{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.Selda
import Database.Selda.PostgreSQL
import ServantWiring

connInfo :: PGConnectInfo
connInfo =
  PGConnectInfo
    { pgHost = "localhost",
      pgPort = 5432,
      pgDatabase = "hitmen",
      pgSchema = Nothing,
      pgUsername = Just "postgres",
      pgPassword = Nothing
    }

port :: Int
port = 8000

main :: IO ()
main = do
  putStrLn "Connecting to Postgres"
  conn <- pgOpen connInfo
  putStrLn $ "Starting server on " ++ show port
  liftIO $ startApp 8000 conn
  putStrLn "Closing Postgres"
  seldaClose conn