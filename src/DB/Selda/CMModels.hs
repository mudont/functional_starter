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

-- Group
data Group = Group
  { id :: ID Group,
    name :: Text
  }
  deriving (Show, Generic)

instance SqlRow Group

group :: Table Group
group = table "auth_group" [#id :- autoPrimary]

$(deriveJSON defaultOptions ''Group)

-- Org
data Org = Org
  { id :: ID Org,
    name :: Text,
    abbrev :: Text,
    administrator_id :: ID User,
    members_group_id :: ID Group
  }
  deriving (Show, Generic)

instance SqlRow Org
org :: Table Org
org = table "tennis_organization" [#id :- autoPrimary]

$(deriveJSON defaultOptions ''Org)

-- League
data League = League
  { id :: ID League,
    name :: Text,
    abbrev :: Maybe Text,
    org_id :: ID Org,
    year :: Int,
    season :: Text,
    division:: Text,
    begin_date :: Day,
    is_knockout :: Bool
  }
  deriving (Show, Generic)

instance SqlRow League
league :: Table League
league = table "tennis_league" [#id :- autoPrimary]

$(deriveJSON defaultOptions ''League)

-- Event
data Event = Event
  { id :: ID Event,
    date :: UTCTime,
    name :: Text,
    org_id :: ID Org,
    event_type :: Text,
    comment :: Text,
    league_id :: ID League
  }
  deriving (Show, Generic)

instance SqlRow Event
event :: Table Event
event = table "tannis_event" [#id :- autoPrimary]

$(deriveJSON defaultOptions ''Event)

-- EventRsvp
data EventRsvp = EventRsvp
  { id :: ID EventRsvp,
    date :: UTCTime,
    name :: Text,
    org_id :: ID Org,
    event_type :: Text,
    comment :: Text,
    league_id :: ID League
  }
  deriving (Show, Generic)

instance SqlRow EventRsvp
eventrsvp :: Table EventRsvp
eventrsvp = table "tannis_eventrsvp" [#id :- autoPrimary]

$(deriveJSON defaultOptions ''EventRsvp)