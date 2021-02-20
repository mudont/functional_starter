{-# LANGUAGE OverloadedStrings #-}

module Handlers.TennisHandler where

import API.TennisApi (TennisApi)
import AppM
import ClassyPrelude (Int, Maybe (Just, Nothing), Text, head, null, pure, show, ($), (.))
import Control.Monad.Except (MonadIO (..))
import Control.Monad.Trans.Reader (asks)
import DB.Models hiding (email)
import qualified DB.Queries as Query
import Database.Selda (Query, Row, SqlRow, query)
import Database.Selda.Backend (runSeldaT)
import Database.Selda.PostgreSQL (PG)
import DjangoPassword
import Protolude (putText, threadDelay, (&))
import Servant
import qualified Servant.Auth.Server as SAS
import Servant.Server ()

users :: AppM [User]
users = dbQuery Query.allUsers

getUser :: Text -> AppM (Maybe User)
getUser un = do
  us <- dbQuery (Query.getUser un)
  pure $ case us of
    [u] -> pure u
    _ -> Nothing

groups :: AppM [Group]
groups = dbQuery Query.allGroups

dbQuery :: SqlRow b => Query PG (Row PG b) -> AppM [b]
dbQuery q = do
  conn <- asks dbConn
  liftIO $ runSeldaT (query q) conn

checkPasswd :: Text -> Text -> AppM (Maybe User)
checkPasswd username pswd = do
  mu <- getUser username
  pure $ case mu of
    Nothing -> Nothing
    Just u -> if validatePassword pswd (password u) then Just u else Nothing

tennisHandler :: ServerT TennisApi AppM
tennisHandler =
  users
    :<|> getUser
    :<|> groups
