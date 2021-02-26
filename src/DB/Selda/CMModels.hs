module DB.Selda.CMModels where

import ClassyPrelude
import Data.Aeson (defaultOptions)
import Data.Aeson.TH (deriveJSON)
-- import Data.Fixed (Pico)
-- import Data.Int (Int32, Int64)
-- import Data.Maybe (fromMaybe, isNothing)
-- import Data.Text (Text, pack)
import Database.Selda hiding (Group)
import Database.Selda.PostgreSQL ()

data SiteType = Cricket | Tennis | Exchange | Other
  deriving (Show, Read, Bounded, Enum)

instance SqlType SiteType

$(deriveJSON defaultOptions ''SiteType)

data User = User
  { id :: ID User,
    username :: Text,
    password :: Text,
    last_login :: Maybe UTCTime,
    first_name :: Text,
    last_name :: Text,
    email :: Maybe Text,
    is_staff :: Bool,
    is_active :: Bool,
    is_superuser :: Bool,
    date_joined :: UTCTime
  }
  deriving (Show, Generic)

instance SqlRow User

user :: Table User
user = table "auth_user" [#id :- autoPrimary]

$(deriveJSON defaultOptions ''User)

data Player = Player
  { id :: ID Player,
    user_id :: ID User,
    mobile_phone :: Maybe Text,
    home_phone :: Maybe Text,
    work_phone :: Maybe Text,
    format_preference :: Maybe Text,
    social_user_id :: Maybe Text
  }
  deriving (Show, Generic)

instance SqlRow Player

player :: Table Player
player = table "tennis_player" [#id :- autoPrimary]

$(deriveJSON defaultOptions ''Player)
