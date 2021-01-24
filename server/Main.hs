{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import CmdArgs
import Config
import Database.Selda
import Database.Selda.PostgreSQL
import ServantWiring

main :: IO ()
main = do
  opts <- cmdArgs
  cfg <- config $ optConfigFile opts
  let pn :: Int = fromIntegral $ port cfg
  putStrLn "Connecting to Postgres"
  conn <-
    pgOpen
      PGConnectInfo
        { pgHost = dbHost cfg,
          pgPort = fromIntegral $ dbPort cfg,
          pgDatabase = database cfg,
          pgSchema = schema cfg,
          pgUsername = dbUsername cfg,
          pgPassword = dbPassword cfg
        }
  putStrLn $ "Starting server on " ++ show pn
  liftIO $ startApp pn conn
  putStrLn "Closing Postgres"
  seldaClose conn