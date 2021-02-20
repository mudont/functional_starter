#!/usr/bin/env stack
-- stack script --resolver lts-17.0

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

---------------------------------------
import ClassyPrelude hiding (group)
import Config
import DB.Models
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Database.Selda
import Database.Selda.PostgreSQL
import Options.Applicative

--import Options.Generic

dropTables :: PGConnectInfo -> IO ()
dropTables connInfo = withPostgreSQL connInfo $ do
  liftIO $ putStrLn "Dropping Tables"
  dropTable person
  dropTable group
  dropTable groupMember
  dropTable site

createTables :: PGConnectInfo -> IO ()
createTables connInfo = withPostgreSQL connInfo $ do
  createTable person
  insert_
    person
    [ Person def "Murali" (Just "R") "Donthireddy" (Just "donthireddy@yahoo.com") (Just "646") "murali" "" def def,
      Person def "C" (Just "M") "Raje" (Just "cmraje@yahoo.com") (Just "732") "cm" "" def def
    ]
  createTable group
  createTable groupMember
  createTable site

data Opts = Opts
  { optGlobalFlag :: !Bool,
    optConfigFile :: !Text,
    optCommand :: !Command
  }
  deriving (Generic, Show)

textOption :: Mod OptionFields String -> Parser T.Text
textOption = fmap T.pack . strOption

data Command
  = Create
  | Drop

main :: IO ()
main = do
  liftIO $ putStrLn "Reading Dhall config to create tables"
  (opts :: Opts) <- execParser optsParser
  cfg <- config $ optConfigFile opts
  let connInfo =
        PGConnectInfo
          { pgHost = dbHost cfg,
            pgPort = fromIntegral $ dbPort cfg,
            pgDatabase = database cfg,
            pgSchema = schema cfg,
            pgUsername = dbUsername cfg,
            pgPassword = dbPassword cfg
          }
   in case optCommand opts of
        Create -> do
          createTables connInfo
          putStrLn "Created Tables"
        Drop -> do
          dropTables connInfo
          putStrLn "Deleted Tables!"
  print ("global flag: " ++ show (optGlobalFlag opts))
  where
    optsParser :: ParserInfo Opts
    optsParser =
      info
        (helper <*> versionOption <*> programOptions)
        ( fullDesc <> progDesc "Database management"
            <> header
              "create/drop tables"
        )
    versionOption :: Parser (a -> a)
    versionOption = infoOption "0.1" (long "version" <> help "Show version")
    programOptions :: Parser Opts
    programOptions =
      Opts <$> switch (long "global-flag" <> help "Set a global flag")
        <*> textOption
          ( long "config" <> metavar "config-file" <> value "./config.dhall"
              <> help "Config file. defaults to ./config.dhall"
          )
        <*> hsubparser (createCommand <> deleteCommand)
    createCommand :: Mod CommandFields Command
    createCommand =
      command
        "create"
        (info (pure Create) (progDesc "Create all tables"))

    deleteCommand :: Mod CommandFields Command
    deleteCommand =
      command
        "drop"
        (info (pure Drop) (progDesc "Drop all tables"))