{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Models where

-- BEgin paste WilliamYao's selda code

import Data.Aeson (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Fixed (Pico)
import Data.Int (Int32, Int64)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text, pack)
import Data.Time
import Database.Selda
import Database.Selda.PostgreSQL

data Handler = Handler
  { handlerID :: ID Handler,
    handlerCodename :: Text,
    handlerCreatedAt :: UTCTime,
    handlerUpdatedAt :: UTCTime
  }
  deriving (Show, Generic)

data Hitman = Hitman
  { hitmanID :: ID Hitman,
    hitmanCodename :: Text,
    hitmanHandlerID :: ID Handler,
    hitmanCreatedAt :: UTCTime,
    hitmanUpdatedAt :: UTCTime
  }
  deriving (Show, Generic)

data Mark = Mark
  { markID :: ID Mark,
    markListBounty :: Int,
    markFirstName :: Text,
    markLastName :: Text,
    markDescription :: Maybe Text,
    markCreatedAt :: UTCTime,
    markUpdatedAt :: UTCTime
  }
  deriving (Show, Generic)

data PursuingMark = PursuingMark
  { pursuingMarkHitmanID :: ID Hitman,
    pursuingMarkMarkID :: ID Mark,
    pursuingMarkCreatedAt :: UTCTime,
    pursuingMarkUpdatedAt :: UTCTime
  }
  deriving (Show, Generic)

data ErasedMark = ErasedMark
  { erasedMarkHitmanID :: ID Hitman,
    erasedMarkMarkID :: ID Mark,
    erasedMarkAwardedBounty :: Int,
    erasedMarkCreatedAt :: UTCTime,
    erasedMarkUpdatedAt :: UTCTime
  }
  deriving (Show, Generic)

instance SqlRow Handler

instance SqlRow Hitman

instance SqlRow Mark

instance SqlRow PursuingMark

instance SqlRow ErasedMark

$(deriveJSON defaultOptions ''Mark)
$(deriveJSON defaultOptions ''Handler)

mapFields :: [(Text, Text)] -> (Text -> Text)
mapFields mappings t = fromMaybe t (lookup t mappings)

-- There's probably a way to get these table definitions to check if the selector
-- actually exists in the datatype, but I don't want to spend that time.

handlers :: Table Handler
handlers =
  tableFieldMod "handlers" [#handlerID :- autoPrimary] $
    mapFields
      [ ("handlerID", "id"),
        ("handlerCodename", "codename"),
        ("handlerCreatedAt", "created_at"),
        ("handlerUpdatedAt", "updated_at")
      ]

hitmen :: Table Hitman
hitmen =
  tableFieldMod "hitmen" [#hitmanID :- autoPrimary] $
    mapFields
      [ ("hitmanID", "id"),
        ("hitmanCodename", "codename"),
        ("hitmanHandlerID", "handler_id"),
        ("hitmanCreatedAt", "created_at"),
        ("hitmanUpdatedAt", "updated_at")
      ]

marks :: Table Mark
marks =
  tableFieldMod "marks" [#markID :- autoPrimary] $
    mapFields
      [ ("markID", "id"),
        ("markListBounty", "list_bounty"),
        ("markFirstName", "first_name"),
        ("markLastName", "last_name"),
        ("markDescription", "description"),
        ("markCreatedAt", "created_at"),
        ("markUpdatedAt", "updated_at")
      ]

pursuingMarks :: Table PursuingMark
pursuingMarks =
  tableFieldMod "pursuing_marks" [(#pursuingMarkHitmanID :+ #pursuingMarkMarkID) :- primary] $
    mapFields
      [ ("pursuingMarkHitmanID", "hitman_id"),
        ("pursuingMarkMarkID", "mark_id"),
        ("pursuingMarkCreatedAt", "created_at"),
        ("pursuingMarkUpdatedAt", "updated_at")
      ]

erasedMarks :: Table ErasedMark
erasedMarks =
  tableFieldMod "erased_marks" [(#erasedMarkHitmanID :+ #erasedMarkMarkID) :- primary] $
    mapFields
      [ ("erasedMarkHitmanID", "hitman_id"),
        ("erasedMarkMarkID", "mark_id"),
        ("erasedMarkAwardedBounty", "awarded_bounty"),
        ("erasedMarkCreatedAt", "created_at"),
        ("erasedMarkUpdatedAt", "updated_at")
      ]
