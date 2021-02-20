{-# LANGUAGE OverloadedStrings #-}

module Handlers.WsHandler where

import API
import AppM
import ClassyPrelude (Int, Maybe (Just, Nothing), Text, show, ($), (.))
import Control.Monad
import Control.Monad.Except (MonadIO (..))
import qualified Data.Text as Text
import Handlers.AuthHandler
import Handlers.RajniHandler
import Handlers.TennisHandler (tennisHandler)
import Network.WebSockets (PendingConnection, RejectRequest (..), acceptRequest, defaultRejectRequest, pendingRequest, rejectRequestWith, sendTextData, withPingThread)
import Protolude (putText, threadDelay)
import Servant
import Servant.Auth ()
import qualified Servant.Auth.Server as SAS
import Servant.Server ()

wsHandler :: Maybe Text -> PendingConnection -> AppM ()
wsHandler = streamData
  where
    -- streamData :: MonadIO m => PendingConnection -> m ()
    streamData :: Maybe Text -> PendingConnection -> AppM ()
    streamData mat pc = liftIO . forM_ ([1 ..] :: [Int]) $ \_i -> do
      -- putText $ "WS Pending conn" <> show (pendingRequest pc)
      case mat of
        Nothing -> rejectRequestWith pc $ defaultRejectRequest {rejectMessage = "Need access-token"}
        Just _jwt -> do
          c <- acceptRequest pc
          liftIO $
            withPingThread c 10 (return ()) $ do
              putText "DBG ping thread "
              forM_ ([1 ..] :: [Int]) $ \i -> do
                -- putText $ "DBG sending " <> show i
                sendTextData c (Text.pack $ show (i :: Int))
                threadDelay 1000000
