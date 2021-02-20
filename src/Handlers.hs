{-# LANGUAGE OverloadedStrings #-}

module Handlers where

import API
import AppM
import Handlers.AuthHandler
import Handlers.RajniHandler
import Handlers.TennisHandler (tennisHandler)
import Handlers.WsHandler (wsHandler)
import Servant
import Servant.Auth ()
import qualified Servant.Auth.Server as SAS
import Servant.Server ()

server :: SAS.CookieSettings -> SAS.JWTSettings -> ServerT (API auths) AppM
server cs jwts =
  authHandler cs jwts
    :<|> tennisHandler
    :<|> rajniServer
    :<|> wsHandler
    :<|> serveDirectoryWebApp "./elm-client/build"
