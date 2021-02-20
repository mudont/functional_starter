module Handlers.RajniHandler where

import API.RajniApi (RajniApi)
import AppM
import ClassyPrelude
import Data.List ((!!))
import Err
import Servant
import qualified Servant.Auth.Server as SAS
import qualified System.Random as Random
import Types

chuckNorrisQuote :: SAS.AuthResult UserData -> AppM String
chuckNorrisQuote (SAS.Authenticated _user) = do
  i <- liftIO $ Random.randomRIO (0, length cQs - 1)
  return (cQs !! i)
chuckNorrisQuote _ = forbidden " Looks like JWT cookie didn't work. No Chuck Norris for you"

rajniQuote :: AppM String
rajniQuote = do
  i <- liftIO $ Random.randomRIO (0, length rQs - 1)
  return (rQs !! i)

rajniServer :: ServerT (RajniApi auths) AppM
rajniServer = chuckNorrisQuote :<|> rajniQuote

cQs :: [String]
cQs =
  [ "Medusa stared at Chuck Norris. She turned into dust",
    "Chuck Norris can divide by zero.",
    "Chuck Norris can kill two stones with one bird.",
    "Chuck Norris once visited the Virgin Islands. They are now The Islands.",
    "When the Boogeyman goes to sleep every night, he checks his closet for Chuck Norris.",
    "Chuck Norris knows Victoria's secret"
  ]

rQs :: [String]
rQs =
  [ "Naan oru thadava sonna nooru thadava sonna maadiri",
    "Ketta payyan saar inda Kaali",
    "Rajni tested +ve for Corona. Corona is now under quarantine",
    "Rajni once told a boy to keep quiet. He became Manmohan Singh",
    "Idhu eppai irukku",
    "See-eeviduven",
    "Naan eppo varuven, epdi varuvennu yarukkum theriyaadu. Aana vara vendiya nerathula"
  ]
