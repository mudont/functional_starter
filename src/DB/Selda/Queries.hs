module DB.Selda.Queries where

import ClassyPrelude hiding (group)
import DB.Selda.CMModels
import qualified DB.Selda.CMModels as CMM
import Data.Fixed (Pico)
import Data.Time
import Database.Selda hiding (Group)
import Database.Selda.Backend
import Database.Selda.PostgreSQL

mkUTCTime ::
  (Integer, Int, Int) ->
  (Int, Int, Pico) ->
  UTCTime
mkUTCTime (year, mon, day) (hour, min, sec) =
  UTCTime
    (fromGregorian year mon day)
    (timeOfDayToTime (TimeOfDay hour min sec))

--------------------
-- QUERIES - Just datastructures. The IO stuff should be moved to Handler.
--------------------
getUser :: Text -> Query s (Row s User)
getUser username = do
  u <- select user
  restrict (u ! #username .== literal username)
  pure u

getUserByEmail :: Text -> Query s (Row s User)
getUserByEmail email = do
  u <- select user
  restrict (u ! #email .== literal (Just email))
  pure u

insertUserQ :: User -> SeldaM PG (ID User)
insertUserQ u =
  insertWithPK user [u]

insertUser :: SeldaConnection PG -> User -> IO (ID User)
insertUser conn u =
  runSeldaT (insertUserQ u) conn

insertPlayerQ :: Player -> SeldaM PG (ID Player)
insertPlayerQ p =
  insertWithPK player [p]

insertPlayer :: SeldaConnection PG -> Player -> IO (ID Player)
insertPlayer conn p =
  runSeldaT (insertPlayerQ p) conn

insertUserPlayerQ :: User -> SeldaM PG (ID User)
insertUserPlayerQ u = do
  uid <- insertUserQ u
  let p = Player def uid Nothing Nothing Nothing Nothing Nothing
  insertPlayerQ p
  pure uid

insertUserPlayer :: SeldaConnection PG -> User -> IO (ID User)
insertUserPlayer conn u =
  runSeldaT (transaction $ insertUserPlayerQ u) conn

allUsers :: Query s (Row s User)
allUsers = select user

allPlayers :: Query s (Row s Player)
allPlayers = select player

-- Join users and players

allUsersPlayers :: Query s (Row s User :*: Row s Player)
allUsersPlayers = do
  u <- select user
  p <- select player
  restrict (u ! #id .== p ! #user_id)
  return (u :*: p)
