module DB.Selda.Queries where

import           ClassyPrelude             hiding (group, id)
import           DB.Selda.CMModels
import qualified DB.Selda.CMModels         as CMM
import           Data.Fixed                (HasResolution (resolution), Pico)
import           Data.String.Conv          (toS)
import           Data.Time
import           Database.Selda            hiding (Group)
import           Database.Selda.Backend
import           Database.Selda.PostgreSQL (PG)
import           Types                     (ContactInfo (email, firstName, homePhone, lastName, mobilePhone, password, username, workPhone))
import           Util.Crypto               (genRandomBS, getRandTxt,
                                            makeDjangoPassword)

-- CREATE TABLES
initDatabase :: SeldaM PG ()
initDatabase  = do
  tryCreateTable org
  tryCreateTable league
  tryCreateTable user
  tryCreateTable group
  tryCreateTable player
  tryCreateTable event
  tryCreateTable eventrsvp
  tryCreateTable resetToken

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

insertResetTokenQ :: Text -> ID User -> SeldaM PG (ID ResetToken)
insertResetTokenQ token uid = do
  currTime <- liftIO getCurrentTime
  let oneDay = 24 * 60 * 60
  let ts = addUTCTime oneDay currTime
  insertWithPK resetToken [ResetToken token uid ts]

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

userInfo :: Text -> Query s (Row s User :*: Row s Player)
userInfo thisUser = do
  u :*: p <- allUsersPlayers
  restrict (u ! #username .== literal thisUser)
  return (u :*: p)

maybeHash :: Maybe Text -> IO (Maybe Text )
maybeHash mp = do
  case mp of
    Nothing -> return Nothing
    Just p ->  do
      h <- makeDjangoPassword p
      return $ Just h

updateUserInfo :: ContactInfo -> SeldaM PG ()
updateUserInfo ci = do
  let mpswd = password (ci::ContactInfo)
  hash <- liftIO $ maybeHash mpswd

  update_ user (\u -> u ! #username .== literal (username (ci::ContactInfo)) )
               (\u -> u `with` [
                 #first_name := literal (firstName ci),
                 #last_name := literal (lastName ci),
                 #email := literal (Just $ email (ci::ContactInfo)),
                 #password := maybe (u ! #password) literal hash
               ])
updatePlayerInfo :: ContactInfo -> SeldaM PG ()
updatePlayerInfo ci = do
  [u] <- query $ getUser (username (ci::ContactInfo))
  update_ player (\p -> p ! #user_id .== literal (id (u::User)) )
               (\u -> u `with` [
                 #mobile_phone := literal (Just $ mobilePhone ci),
                 #home_phone := literal (Just $ homePhone ci),
                 #work_phone := literal (Just $ workPhone ci)
               ])
updateProfile :: ContactInfo -> SeldaM PG ()
updateProfile ci = do
  transaction $ do
    updateUserInfo ci
    updatePlayerInfo ci

-- PASSWORD RESET

createResetSecret :: Text -> SeldaM PG (Maybe Text)
createResetSecret email = do
  mu <- query $ getUserByEmail email
  case mu of
    [u] -> do
      tokenBs <- liftIO  genRandomBS
      let token = toS tokenBs
      insertResetTokenQ (toS token) (id (u::User))
      return $ Just token
    [] -> return Nothing


getUserFromResetToken :: Text -> Query s (Row s User)
getUserFromResetToken token = do
  t <- select resetToken
  u <- select user
  restrict (t ! #user_id .== u ! #id )
  restrict (t ! #token .== literal token)
  pure u

