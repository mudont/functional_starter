#!/usr/bin/env stack
-- stack script --resolver lts-17.0

{-# LANGUAGE OverloadedStrings #-}

--import ClassyPrelude

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Text
import Data.Time
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

--import Prelude

data User = User
  { userId :: Int,
    username :: Text,
    firstName :: Text,
    lastLogin :: Maybe LocalTime
  }
  deriving (Show)

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field

instance ToRow User where
  toRow t =
    [ toField (userId t),
      toField (username t),
      toField (firstName t),
      toField (lastLogin t)
    ]

getUsers :: Connection -> IO [User]
getUsers c = query_ c "select id, username, first_name, last_login from auth_user"

hello :: IO ()
hello = do
  conn <- connectPostgreSQL "dbname='tennis'"
  us <- (getUsers conn)
  forM_ us $ \u ->
    putStrLn $ show u

main :: IO ()
main = hello