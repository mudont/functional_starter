{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO)
import Database.Selda
import Database.Selda.Backend
import Database.Selda.PostgreSQL
import Models
import Queries

connInfo :: PGConnectInfo
connInfo =
  PGConnectInfo
    { pgHost = "localhost",
      pgPort = 5432,
      pgDatabase = "hitmen",
      pgSchema = Nothing,
      pgUsername = Just "postgres",
      pgPassword = Nothing
    }

main :: IO ()
main = do
  conn <- pgOpen connInfo
  latest <- liftIO $ runSeldaT (query latestHits) conn
  totalBountyFor1' <- liftIO $ runSeldaT (query (totalBountyAwarded (toId 1))) conn

  liftIO $ runSeldaT (increaseListBounty 1337 (toId 4)) conn

  liftIO $ putStrLn "===== LATEST HITS ====="
  liftIO $ print latest

  liftIO $ putStrLn "===== TOTAL BOUNTY AWARDED TO first HITMAN ====="
  liftIO $ print totalBountyFor1'
  seldaClose conn
  liftIO $ putStrLn "===== Done ===="

main1 :: IO ()
main1 = withPostgreSQL connInfo $ do
  latest' <- query latestHits
  totalBountyFor1' <- query (totalBountyAwarded (toId 1))

  increaseListBounty 1337 (toId 4)

  liftIO $ putStrLn "===== LATEST HITS ====="
  liftIO $ print latest'

  liftIO $ putStrLn "===== TOTAL BOUNTY AWARDED TO HITMAN 1 ====="
  liftIO $ print totalBountyFor1'

data Pet = Dog | Horse | Dragon
  deriving (Show, Read, Bounded, Enum)

instance SqlType Pet

data Person = Person
  { name :: Text,
    age :: Int,
    pet :: Maybe Pet
  }
  deriving (Show, Generic)

instance SqlRow Person

people :: Table Person
people = table "people" [#name :- primary]

main2 :: IO ()
main2 = withPostgreSQL connInfo $ do
  createTable people
  insert_
    people
    [ Person "Velvet" 19 (Just Dog),
      Person "Kobayashi" 23 (Just Dragon),
      Person "Miyu" 10 Nothing
    ]

  adultsAndTheirPets <- query $ do
    person <- select people
    restrict (person ! #age .>= 18)
    return (person ! #name :*: person ! #pet)
  liftIO $ print adultsAndTheirPets
