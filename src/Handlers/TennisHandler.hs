{-# LANGUAGE OverloadedStrings #-}

module Handlers.TennisHandler where

import API.TennisApi (TennisApi)
import AppM
import ClassyPrelude hiding (asks)
import Control.Monad.Except (MonadIO (..))
import Control.Monad.Trans.Reader (asks)
import DB.Selda.CMModels hiding (email)
import qualified DB.Selda.CMModels as CMM
import qualified DB.Selda.Queries as Query
import Database.Selda (Query, Row, SqlRow, def, query, transaction)
import Database.Selda.Backend (runSeldaT)
import Database.Selda.PostgreSQL (PG)
import Err
import Protolude (putText, threadDelay, toS, (&))
import Servant
import qualified Servant.Auth.Server as SAS
import Servant.Server ()
import Types (UserData (email), getUserName, modUserName)
import Util.Crypto

-- Servant Handlers

tennisHandler :: ServerT TennisApi AppM
tennisHandler =
  users
    :<|> getUser
    :<|> players

-- Get data from database

users :: AppM [User]
users = dbQuery Query.allUsers

getUser :: Text -> AppM (Maybe User)
getUser un = do
  us <- dbQuery (Query.getUser un)
  pure $ case us of
    [u] -> pure u
    _ -> Nothing

players :: AppM [Player]
players = dbQuery Query.allPlayers

-- usersPlayers :: AppM [UserPlayer]
--usersPlayers = dbQuery Query.allUsersPlayers

-- | Create user in database if not already present
-- | This is useful for registering users authenticated thru OIDC
ensureDBUser :: UserData -> AppM UserData
ensureDBUser uD = do
  mu <- dbQuery $ Query.getUserByEmail (Types.email (uD :: UserData))
  case mu of
    [dbUser] -> do
      let un = CMM.username (dbUser :: User)
      let (userData :: UserData) = Types.modUserName un uD
      pure userData
    _ -> do
      -- if email, strip domain part
      let un = takeWhile (/= '@') $ getUserName uD
      let em = email (uD :: UserData)
      eU <- createUser un Nothing em
      case eU of
        Left err -> forbidden err
        Right _ -> pure $ Types.modUserName un uD

checkPasswd :: Text -> Text -> AppM (Maybe User)
checkPasswd username pswd = do
  mu <- getUser username
  pure $ case mu of
    Nothing -> Nothing
    Just u -> if validatePassword pswd (password (u :: User)) then Just u else Nothing

createUser :: Text -> Maybe Text -> Text -> AppM (Either Text User)
createUser username pswd email = do
  mu <- getUser username
  case mu of
    Nothing -> do
      ts <- liftIO getCurrentTime
      conn <- asks dbConn
      hashed <- liftIO $ case pswd of
        Just p -> makeDjangoPassword p
        _ -> pure "-- No Password. User created from OIDC login --"

      let u = User def username hashed Nothing "" "" (Just email) True True False ts
      liftIO $ Query.insertUserPlayer conn u
      pure $ Right u
    Just _ -> pure $ Left $ "User " <> username <> " exists"

dbQuery :: SqlRow b => Query PG (Row PG b) -> AppM [b]
dbQuery q = do
  conn <- asks dbConn
  liftIO $ runSeldaT (query q) conn
