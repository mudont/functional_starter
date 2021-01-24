{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module ServantWiring where

-- ( startApp,
--   app,
-- )

import API
import Control.Exception
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Control.Natural
import Data.Aeson (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Database.Selda hiding (Group)
import Database.Selda.Backend (SeldaConnection, runSeldaT)
import Database.Selda.PostgreSQL
import Database.Selda.SqlType (toId)
import Models hiding (handlers, marks)
import Network.Wai.Handler.Warp (run)
import Queries
import Servant

startApp :: Int -> SeldaConnection a -> IO ()
startApp port conn = run port $ app conn -- Wai.run

app :: SeldaConnection a -> Application -- Wai Application
app conn = serve api $ server conn

api :: Proxy API
api = Proxy

server :: SeldaConnection a -> Server API
server conn =
  people conn
    :<|> groups conn
    :<|> serveDirectoryWebApp "."

{-
For an idiomatic way to avoid boilerplate of dbQuery prefix in each
API handler function below, see
https://www.servant.dev/posts/2017-03-03-servant-and-db.html
It shows how to create a Monad in a transformer stack and use hoistServer
-}
people :: SeldaConnection a -> Servant.Handler [Person]
people conn = dbQuery conn allPeople

groups :: SeldaConnection a -> Servant.Handler [Group]
groups conn = dbQuery conn allGroups

dbQuery :: SqlRow b => SeldaConnection a -> Query a (Row a b) -> Servant.Handler [b]
dbQuery conn q =
  convert $
    liftIO $
      runSeldaT
        (query q)
        conn

-- https://www.parsonsmatt.org/2017/06/21/exceptional_servant_handling.html
-- The more fancy natural transformation in the last step didn't work but
-- it was useful
-- convert' :: IO ~> Servant.Handler
-- convert' = NT . convert
convert :: IO a -> Servant.Handler a
convert = Servant.Handler . ExceptT . Control.Exception.try

{-
Couldn't match expected type ‘IO :~> Handler’
              with actual type ‘IO a1 -> f0 :~> g0’
• Probable cause: ‘(.)’ is applied to too few arguments
  In the expression: NT . Handler . ExceptT . try
  In an equation for ‘convert’:
      convert = NT . Handler . ExceptT . try
-}