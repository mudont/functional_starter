{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Config where

import ClassyPrelude
import Dhall
---------------------------------------
import Models
import Options.Applicative

data MyConfig = MyConfig
  { port :: Natural,
    dbHost :: Text,
    dbPort :: Natural,
    database :: Text,
    schema :: Maybe Text,
    dbUsername :: Maybe Text,
    dbPassword :: Maybe Text
  }
  deriving (Generic, Show)

instance FromDhall MyConfig

config :: Text -> IO MyConfig
config configFile = do
  putStrLn "Reading Dhall config "
  cfg :: MyConfig <- input Dhall.auto configFile
  pure cfg