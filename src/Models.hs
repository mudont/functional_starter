{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Models where

import Data.Aeson (defaultOptions)
import Data.Aeson.TH (deriveJSON)
-- import Data.Fixed (Pico)
-- import Data.Int (Int32, Int64)
-- import Data.Maybe (fromMaybe, isNothing)
-- import Data.Text (Text, pack)
import Data.Time
import Database.Selda hiding (Group)
import Database.Selda.PostgreSQL

data SiteType = Cricket | Tennis | Exchange
  deriving (Show, Read, Bounded, Enum)

instance SqlType SiteType

$(deriveJSON defaultOptions ''SiteType)

data Person = Person
  { person_id :: ID Person,
    first_name :: Text,
    middle_name :: Maybe Text,
    last_name :: Text,
    email :: Maybe Text,
    phone :: Maybe Text,
    login :: Text,
    password :: Text,
    per_created_at :: UTCTime,
    per_updated_at :: UTCTime
  }
  deriving (Show, Generic)

instance SqlRow Person

person :: Table Person
person = table "person" [#person_id :- autoPrimary]

$(deriveJSON defaultOptions ''Person)

data Group = Group
  { grp_id :: ID Group,
    grp_name :: Text,
    grp_created_at :: UTCTime,
    grp_updated_at :: UTCTime
  }
  deriving (Show, Generic)

instance SqlRow Group

$(deriveJSON defaultOptions ''Group)

group :: Table Group
group = table "group" [#grp_id :- autoPrimary]

data GroupMember = GroupMember
  { gm_person_id :: ID Person,
    gm_grp_id :: ID Group,
    gm_created_at :: UTCTime,
    gm_updated_at :: UTCTime
  }
  deriving (Show, Generic)

instance SqlRow GroupMember

$(deriveJSON defaultOptions ''GroupMember)

groupMember :: Table GroupMember
groupMember = table "group_member" [(#gm_person_id :+ #gm_grp_id) :- primary]

data Site = Site
  { site_id :: ID Site,
    site_name :: Text,
    site_owener :: ID Person,
    site_type :: SiteType,
    site_created_at :: UTCTime,
    site_updated_at :: UTCTime
  }
  deriving (Show, Generic)

instance SqlRow Site

$(deriveJSON defaultOptions ''Site)

site :: Table Site
site = table "site" [#site_id :- primary] --[(#pursuingMarkHitmanID :+ #pursuingMarkMarkID) :- primary]

-- mapFields :: [(Text, Text)] -> (Text -> Text)
-- mapFields mappings t = fromMaybe t (lookup t mappings)

-- -- There's probably a way to get these table definitions to check if the selector
-- -- actually exists in the datatype, but I don't want to spend that time.

-- handlers :: Table Handler
-- handlers =
--   tableFieldMod "handlers" [#handlerID :- autoPrimary] $
--     mapFields
--       [ ("handlerID", "id"),
--         ("handlerCodename", "codename"),
--         ("handlerCreatedAt", "created_at"),
--         ("handlerUpdatedAt", "updated_at")
--       ]
