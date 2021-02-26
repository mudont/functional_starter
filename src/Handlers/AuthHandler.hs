{-# LANGUAGE OverloadedStrings #-}

module Handlers.AuthHandler where

import API.AuthApi
import AppM
import ClassyPrelude (Applicative (pure), ByteString, Either (Left, Right), Eq ((==)), IO, Int, IsSequence (drop, take), IsString (fromString), Map, Maybe (Just, Nothing), Num ((+), (-)), Ord, Show, Text, Utf8 (decodeUtf8, encodeUtf8), const, either, fromMaybe, length, show, swap, ($), (.), (<$>), (<>))
import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans.Reader (asks)
import Crypto.Random.API (CPRG, cprgGenBytes)
import DB.Selda.CMModels
import "base64-bytestring" Data.ByteString.Base64.URL (encode)
import Data.IORef
  ( IORef,
    atomicModifyIORef',
    readIORef,
  )
import Data.List (head)
import qualified Data.List as List
import qualified Data.Map as M
import Data.String (String)
import Err
import Handlers.TennisHandler (checkPasswd, createUser, getUser)
import Jose.Jwt
  ( Jwt (..),
    decodeClaims,
  )
import Protolude (putText, (&))
import Protolude.Conv
import Servant
import Servant.Auth ()
import qualified Servant.Auth.Server as SAS
import Servant.Server ()
import qualified System.Random as Random
import Types
import Util.Email (mail)
import qualified Web.OIDC.Client as O

authHandler :: SAS.CookieSettings -> SAS.JWTSettings -> ServerT AuthApi AppM
authHandler cs jwts =
  handleLogin
    :<|> handleLoggedIn cs jwts
    :<|> handlePasswordLogin cs jwts
    :<|> handleRegistration cs jwts

redirects :: (StringConv s ByteString) => s -> AppM ()
redirects url = throwError err302 {errHeaders = [("Location", toS url)]}

genOIDCURL :: AppM ByteString
genOIDCURL = do
  OIDCEnv {..} <- asks oidcEnv
  sid <- genSessionId cprg
  let store = sessionStoreFromSession cprg ssm sid
  loc <- liftIO $ O.prepareAuthenticationRequestUrl store oidc [O.email, O.profile] []
  return $ strToBS (show loc)

gen :: CPRG a => IORef a -> IO ByteString
gen cprg = encode <$> atomicModifyIORef' cprg (swap . cprgGenBytes 64)

genSessionId :: (MonadIO m, CPRG a) => IORef a -> m Text
genSessionId cprg = liftIO $ decodeUtf8 <$> gen cprg

genBytes :: (MonadIO m, CPRG a) => IORef a -> m ByteString
genBytes cprg = liftIO $ gen cprg

saveState :: (MonadIO m, Ord k, Show k, Show a, Show b) => IORef (Map k (a, b)) -> k -> a -> b -> m ()
saveState ssm sid st nonce = do
  --putText $ "DBG saveState key: " <> show sid <> " state: " <> show st <> " nonce: " <> show nonce
  liftIO $ atomicModifyIORef' ssm $ \m -> (M.insert sid (st, nonce) m, ())

getStateBy :: (MonadIO m, Ord k) => IORef (Map k (a1, a2)) -> k -> m (Maybe a1, Maybe a2)
getStateBy ssm sid = liftIO $ do
  m <- M.lookup sid <$> readIORef ssm
  return $ case m of
    Just (st, nonce) -> (Just st, Just nonce)
    _ -> (Nothing, Nothing)

deleteState :: (MonadIO m, Ord k, Show k) => IORef (Map k a) -> k -> m ()
deleteState ssm sid = do
  --putText $ "DBG deleteState key: " <> show sid
  liftIO $ atomicModifyIORef' ssm $ \m -> (M.delete sid m, ())

sessionStoreFromSession :: (MonadIO m, CPRG a, Ord k, Show k) => IORef a -> IORef (Map k (O.State, O.Nonce)) -> k -> O.SessionStore m
sessionStoreFromSession cprg ssm sid =
  O.SessionStore
    { sessionStoreGenerate = genBytes cprg,
      sessionStoreSave = saveState ssm sid,
      sessionStoreGet = getStateBy ssm sid,
      sessionStoreDelete = deleteState ssm sid
    }

strToBS :: String -> ByteString
strToBS = strConv Lenient

handleLogin :: AppM NoContent
handleLogin = do
  loc <- genOIDCURL
  redirects loc
  return NoContent

handleLoggedIn ::
  SAS.CookieSettings ->
  SAS.JWTSettings ->
  -- | error
  Maybe Text ->
  -- | code
  Maybe Text ->
  -- | state
  Maybe Text ->
  AppM (Headers '[Header "Set-Cookie" SAS.SetCookie, Header "Set-Cookie" SAS.SetCookie] UserData)
handleLoggedIn cs jwts err mcode mstate = do
  oidcenv <- asks oidcEnv
  case err of
    Just errorMsg -> forbidden errorMsg
    Nothing -> case mcode of
      Just oauthCode -> do
        sessions <- liftIO $ readIORef (ssm oidcenv)
        {-
          state appears to be the one field connecting the original OIDC redirect URL
          and the eventual callback URL after successful auth flow.
          We look up the entry in our Session that has the same state value
          NOTE: THe sessions map entries are added and removed by the OIDC library
          We should have more than one entry only if mutliple users are concurrently
          authenticating.
        -}
        let (state :: O.State) = encodeUtf8 $ fromMaybe "" mstate
        --let sesskeys = M.keys sessions
        --putText "DBG"
        -- putText $ show $ length sesskeys
        let sid = head $ M.keys $ M.filter (\(st, _) -> st == state) sessions
        putText $ sid <> " sid"
        let store = sessionStoreFromSession (cprg oidcenv) (ssm oidcenv) sid
        --putText $ show $ ">>> Received state = " <> state
        tokens :: (O.Tokens AuthInfo) <- liftIO $ O.getValidTokens store (oidc oidcenv) (mgr oidcenv) state (encodeUtf8 oauthCode)
        --tokens::(O.Tokens AuthInfo) <- liftIO $ O.requestTokens (oidc oidcenv) (Just nonceUglyUglyHack) (toS oauthCode) (mgr oidcenv)
        -- putText . show . O.otherClaims . O.idToken $ tokens
        let jwt = unJwt . O.idTokenJwt $ tokens
            eAuthInfo = decodeClaims jwt :: Either O.JwtError (O.JwtHeader, AuthInfo)
        case eAuthInfo of
          Left jwtErr -> forbidden $ fromString $ "JWT decode/check problem: " <> show jwtErr
          Right (_, authInfo) ->
            if emailVerified authInfo
              then do
                eUser <- liftIO $ handleOIDCLogin authInfo
                case eUser of
                  Left err -> forbidden $ fromString $ "Cant get user: " <> show err
                  Right user -> do
                    acceptUser cs jwts user
              else forbidden "Please verify your email"
      Nothing -> do
        liftIO $ putText "No code param"
        forbidden "no code parameter given"

-- | Called by both OIDC/OAuth2 login as well as Password login flows.
acceptUser ::
  SAS.CookieSettings ->
  SAS.JWTSettings ->
  UserData ->
  AppM (Headers '[Header "Set-Cookie" SAS.SetCookie, Header "Set-Cookie" SAS.SetCookie] UserData)
acceptUser cs jwts user = do
  putText $ "wooo hoo!!!! DBG whooo!!!! Accepting login: " <> toS (show cs :: String)
  jwt <- liftIO $ SAS.makeJWT user jwts Nothing
  let accessToken = either (const ("" :: Text)) toS jwt
  putText $ "access-token: " <> accessToken
  let user' = user {accessToken = Just accessToken}
  mApplyCookies <- liftIO $ SAS.acceptLogin cs jwts user'
  case mApplyCookies of
    Nothing -> do
      putText "DBG mApplyCookies ==  Nothing"
      forbidden "Accept login failed"
    Just applyCookies -> do
      putText "DBG mApplyCookies worked"
      return $ applyCookies user'

genRandomBS :: IO ByteString
genRandomBS = do
  g <- Random.newStdGen
  Random.randomRs (0, n) g & take 42 & fmap toChar & readable 0 & toS & return
  where
    n = length letters - 1
    toChar i = letters List.!! i
    letters = ['A' .. 'Z'] <> ['0' .. '9'] <> ['a' .. 'z']
    readable :: Int -> String -> String
    readable _ [] = []
    readable i str =
      let blocksize = case n of
            0 -> 8
            1 -> 4
            2 -> 4
            3 -> 4
            _ -> 12
          block = take blocksize str
          rest = drop blocksize str
       in if List.null rest
            then str
            else block <> "-" <> readable (i + 1) rest

customerFromAuthInfo :: AuthInfo -> IO Customer
customerFromAuthInfo authinfo = do
  apikey <- genRandomBS
  return
    Customer
      { account = toS (email (authinfo :: AuthInfo)),
        apiKey = apikey,
        mail = Just (toS (email (authinfo :: AuthInfo))),
        fullname = Just (toS (name (authinfo :: AuthInfo))),
        cPicture = Just (toS (picture authinfo))
      }

handleRegistration ::
  SAS.CookieSettings ->
  SAS.JWTSettings ->
  RegistrationForm ->
  AppM (Headers '[Header "Set-Cookie" SAS.SetCookie, Header "Set-Cookie" SAS.SetCookie] UserData)
handleRegistration cs jwt rf = do
  let email' = email (rf :: RegistrationForm)
  eu <- createUser (username (rf :: RegistrationForm)) (password (rf :: RegistrationForm)) email'
  case eu of
    Left err -> forbidden $ "Registration failed: " <> err
    Right u -> do
      liftIO $ Util.Email.mail email' "Welcome to CM Hacker" "Welcome"
      acceptUser cs jwt $ UserData (username (u :: User)) "" "" "" Nothing Nothing

handlePasswordLogin ::
  SAS.CookieSettings ->
  SAS.JWTSettings ->
  LoginForm ->
  AppM (Headers '[Header "Set-Cookie" SAS.SetCookie, Header "Set-Cookie" SAS.SetCookie] UserData)
handlePasswordLogin cs jwt lf = do
  mu <- checkPasswd (username (lf :: LoginForm)) (password (lf :: LoginForm))
  case mu of
    Nothing -> forbidden $ "Invalid password for " <> username (lf :: LoginForm)
    Just u -> acceptUser cs jwt $ UserData (username (u :: User)) "" "" "" Nothing Nothing

type LoginHandler = AuthInfo -> IO (Either Text UserData)

handleOIDCLogin :: LoginHandler
handleOIDCLogin authInfo = do
  custInfo <- liftIO $ customerFromAuthInfo authInfo
  if emailVerified authInfo
    then return . Right . customerToUser $ custInfo
    else return (Left "You emails is not verified by your provider. Please verify your email.")
  where
    customerToUser :: Customer -> UserData
    customerToUser c =
      UserData
        { userId = toS (account c),
          userSecret = toS (apiKey c),
          redirectUrl = Nothing,
          localStorageKey = "api-key",
          uPicture = toS $ fromMaybe "" (cPicture c),
          accessToken = Nothing
        }

--
