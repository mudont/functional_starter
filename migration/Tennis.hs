{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Database where

import qualified Data.Aeson                 as JSON
import           Data.Profunctor
import           Data.Profunctor.Product
import           Data.Profunctor.Product.Default
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Scientific
import           Data.Text
import           Data.Time
import           Data.UUID
import           GHC.Int
import           Opaleye hiding (fromNullable)

-- | A newtype around @a -> Maybe b@ to facilitate conversions from the
-- Nullable types.
newtype ToMaybe a b = ToMaybe { unToMaybe :: a -> Maybe b }

instance Profunctor ToMaybe where
  dimap f g (ToMaybe h) = ToMaybe (fmap g . h . f)

instance ProductProfunctor ToMaybe where
  empty = ToMaybe pure
  (ToMaybe f) ***! (ToMaybe g) = ToMaybe (\(x, y) -> (,) <$> f x <*> g y)

-- | This instance makes sure that values which are required in the output are
-- required in the input.
instance Default ToMaybe (Maybe a) a where
  def = ToMaybe id

-- | This instance allows values which are optional in the output to be
-- optional in the input.
instance Default ToMaybe (Maybe a) (Maybe a) where
  def = ToMaybe pure

-- | Convert from any Nullable type by "sequencing" over all the fields.
fromNullable :: Default ToMaybe a b => a -> Maybe b
fromNullable = unToMaybe def

---- Types for table: account_account ----

data AccountAccount' c1 c2 c3 c4 =
  AccountAccount
    { accountAccountId :: c1
    , accountAccountUserId :: c2
    , accountAccountTimezone :: c3
    , accountAccountLanguage :: c4
    }

type AccountAccount = AccountAccount' Int32 Int32 Text Text

type AccountAccountReadColumns = AccountAccount' (Column PGInt4) (Column PGInt4) (Column PGText) (Column PGText)

type AccountAccountWriteColumns = AccountAccount' (Maybe (Column PGInt4)) (Column PGInt4) (Column PGText) (Column PGText)

type AccountAccountNullableColumns = AccountAccount' (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText))

type AccountAccountNullable = AccountAccount' (Maybe Int32) (Maybe Int32) (Maybe Text) (Maybe Text)

fromNullableAccountAccount :: AccountAccountNullable -> Maybe AccountAccount
fromNullableAccountAccount = fromNullable

$(makeAdaptorAndInstance "pAccountAccount" ''AccountAccount')

accountAccountTable :: Table AccountAccountWriteColumns AccountAccountReadColumns
accountAccountTable = Table "account_account" (pAccountAccount
  AccountAccount
    { accountAccountId = optional "id"
    , accountAccountUserId = required "user_id"
    , accountAccountTimezone = required "timezone"
    , accountAccountLanguage = required "language"
    }
  )

---- Types for table: account_accountdeletion ----

data AccountAccountdeletion' c1 c2 c3 c4 c5 =
  AccountAccountdeletion
    { accountAccountdeletionId :: c1
    , accountAccountdeletionUserId :: c2
    , accountAccountdeletionEmail :: c3
    , accountAccountdeletionDateRequested :: c4
    , accountAccountdeletionDateExpunged :: c5
    }

type AccountAccountdeletion = AccountAccountdeletion' Int32 (Maybe Int32) Text LocalTime (Maybe LocalTime)

type AccountAccountdeletionReadColumns = AccountAccountdeletion' (Column PGInt4) (Column (Nullable PGInt4)) (Column PGText) (Column PGTimestamp) (Column (Nullable PGTimestamp))

type AccountAccountdeletionWriteColumns = AccountAccountdeletion' (Maybe (Column PGInt4)) (Maybe (Column (Nullable PGInt4))) (Column PGText) (Column PGTimestamp) (Maybe (Column (Nullable PGTimestamp)))

type AccountAccountdeletionNullableColumns = AccountAccountdeletion' (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGTimestamp)) (Column (Nullable PGTimestamp))

type AccountAccountdeletionNullable = AccountAccountdeletion' (Maybe Int32) (Maybe Int32) (Maybe Text) (Maybe LocalTime) (Maybe LocalTime)

fromNullableAccountAccountdeletion :: AccountAccountdeletionNullable -> Maybe AccountAccountdeletion
fromNullableAccountAccountdeletion = fromNullable

$(makeAdaptorAndInstance "pAccountAccountdeletion" ''AccountAccountdeletion')

accountAccountdeletionTable :: Table AccountAccountdeletionWriteColumns AccountAccountdeletionReadColumns
accountAccountdeletionTable = Table "account_accountdeletion" (pAccountAccountdeletion
  AccountAccountdeletion
    { accountAccountdeletionId = optional "id"
    , accountAccountdeletionUserId = optional "user_id"
    , accountAccountdeletionEmail = required "email"
    , accountAccountdeletionDateRequested = required "date_requested"
    , accountAccountdeletionDateExpunged = optional "date_expunged"
    }
  )

---- Types for table: account_emailaddress ----

data AccountEmailaddress' c1 c2 c3 c4 c5 =
  AccountEmailaddress
    { accountEmailaddressId :: c1
    , accountEmailaddressUserId :: c2
    , accountEmailaddressEmail :: c3
    , accountEmailaddressVerified :: c4
    , accountEmailaddressPrimary :: c5
    }

type AccountEmailaddress = AccountEmailaddress' Int32 Int32 Text Bool Bool

type AccountEmailaddressReadColumns = AccountEmailaddress' (Column PGInt4) (Column PGInt4) (Column PGText) (Column PGBool) (Column PGBool)

type AccountEmailaddressWriteColumns = AccountEmailaddress' (Maybe (Column PGInt4)) (Column PGInt4) (Column PGText) (Column PGBool) (Column PGBool)

type AccountEmailaddressNullableColumns = AccountEmailaddress' (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGBool)) (Column (Nullable PGBool))

type AccountEmailaddressNullable = AccountEmailaddress' (Maybe Int32) (Maybe Int32) (Maybe Text) (Maybe Bool) (Maybe Bool)

fromNullableAccountEmailaddress :: AccountEmailaddressNullable -> Maybe AccountEmailaddress
fromNullableAccountEmailaddress = fromNullable

$(makeAdaptorAndInstance "pAccountEmailaddress" ''AccountEmailaddress')

accountEmailaddressTable :: Table AccountEmailaddressWriteColumns AccountEmailaddressReadColumns
accountEmailaddressTable = Table "account_emailaddress" (pAccountEmailaddress
  AccountEmailaddress
    { accountEmailaddressId = optional "id"
    , accountEmailaddressUserId = required "user_id"
    , accountEmailaddressEmail = required "email"
    , accountEmailaddressVerified = required "verified"
    , accountEmailaddressPrimary = required "primary"
    }
  )

---- Types for table: account_emailconfirmation ----

data AccountEmailconfirmation' c1 c2 c3 c4 c5 =
  AccountEmailconfirmation
    { accountEmailconfirmationId :: c1
    , accountEmailconfirmationEmailAddressId :: c2
    , accountEmailconfirmationCreated :: c3
    , accountEmailconfirmationSent :: c4
    , accountEmailconfirmationKey :: c5
    }

type AccountEmailconfirmation = AccountEmailconfirmation' Int32 Int32 LocalTime (Maybe LocalTime) Text

type AccountEmailconfirmationReadColumns = AccountEmailconfirmation' (Column PGInt4) (Column PGInt4) (Column PGTimestamp) (Column (Nullable PGTimestamp)) (Column PGText)

type AccountEmailconfirmationWriteColumns = AccountEmailconfirmation' (Maybe (Column PGInt4)) (Column PGInt4) (Column PGTimestamp) (Maybe (Column (Nullable PGTimestamp))) (Column PGText)

type AccountEmailconfirmationNullableColumns = AccountEmailconfirmation' (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGTimestamp)) (Column (Nullable PGTimestamp)) (Column (Nullable PGText))

type AccountEmailconfirmationNullable = AccountEmailconfirmation' (Maybe Int32) (Maybe Int32) (Maybe LocalTime) (Maybe LocalTime) (Maybe Text)

fromNullableAccountEmailconfirmation :: AccountEmailconfirmationNullable -> Maybe AccountEmailconfirmation
fromNullableAccountEmailconfirmation = fromNullable

$(makeAdaptorAndInstance "pAccountEmailconfirmation" ''AccountEmailconfirmation')

accountEmailconfirmationTable :: Table AccountEmailconfirmationWriteColumns AccountEmailconfirmationReadColumns
accountEmailconfirmationTable = Table "account_emailconfirmation" (pAccountEmailconfirmation
  AccountEmailconfirmation
    { accountEmailconfirmationId = optional "id"
    , accountEmailconfirmationEmailAddressId = required "email_address_id"
    , accountEmailconfirmationCreated = required "created"
    , accountEmailconfirmationSent = optional "sent"
    , accountEmailconfirmationKey = required "key"
    }
  )

---- Types for table: account_passwordexpiry ----

data AccountPasswordexpiry' c1 c2 c3 =
  AccountPasswordexpiry
    { accountPasswordexpiryId :: c1
    , accountPasswordexpiryExpiry :: c2
    , accountPasswordexpiryUserId :: c3
    }

type AccountPasswordexpiry = AccountPasswordexpiry' Int32 Int32 Int32

type AccountPasswordexpiryReadColumns = AccountPasswordexpiry' (Column PGInt4) (Column PGInt4) (Column PGInt4)

type AccountPasswordexpiryWriteColumns = AccountPasswordexpiry' (Maybe (Column PGInt4)) (Column PGInt4) (Column PGInt4)

type AccountPasswordexpiryNullableColumns = AccountPasswordexpiry' (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4))

type AccountPasswordexpiryNullable = AccountPasswordexpiry' (Maybe Int32) (Maybe Int32) (Maybe Int32)

fromNullableAccountPasswordexpiry :: AccountPasswordexpiryNullable -> Maybe AccountPasswordexpiry
fromNullableAccountPasswordexpiry = fromNullable

$(makeAdaptorAndInstance "pAccountPasswordexpiry" ''AccountPasswordexpiry')

accountPasswordexpiryTable :: Table AccountPasswordexpiryWriteColumns AccountPasswordexpiryReadColumns
accountPasswordexpiryTable = Table "account_passwordexpiry" (pAccountPasswordexpiry
  AccountPasswordexpiry
    { accountPasswordexpiryId = optional "id"
    , accountPasswordexpiryExpiry = required "expiry"
    , accountPasswordexpiryUserId = required "user_id"
    }
  )

---- Types for table: account_passwordhistory ----

data AccountPasswordhistory' c1 c2 c3 c4 =
  AccountPasswordhistory
    { accountPasswordhistoryId :: c1
    , accountPasswordhistoryPassword :: c2
    , accountPasswordhistoryTimestamp :: c3
    , accountPasswordhistoryUserId :: c4
    }

type AccountPasswordhistory = AccountPasswordhistory' Int32 Text UTCTime Int32

type AccountPasswordhistoryReadColumns = AccountPasswordhistory' (Column PGInt4) (Column PGText) (Column PGTimestamptz) (Column PGInt4)

type AccountPasswordhistoryWriteColumns = AccountPasswordhistory' (Maybe (Column PGInt4)) (Column PGText) (Column PGTimestamptz) (Column PGInt4)

type AccountPasswordhistoryNullableColumns = AccountPasswordhistory' (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGTimestamptz)) (Column (Nullable PGInt4))

type AccountPasswordhistoryNullable = AccountPasswordhistory' (Maybe Int32) (Maybe Text) (Maybe UTCTime) (Maybe Int32)

fromNullableAccountPasswordhistory :: AccountPasswordhistoryNullable -> Maybe AccountPasswordhistory
fromNullableAccountPasswordhistory = fromNullable

$(makeAdaptorAndInstance "pAccountPasswordhistory" ''AccountPasswordhistory')

accountPasswordhistoryTable :: Table AccountPasswordhistoryWriteColumns AccountPasswordhistoryReadColumns
accountPasswordhistoryTable = Table "account_passwordhistory" (pAccountPasswordhistory
  AccountPasswordhistory
    { accountPasswordhistoryId = optional "id"
    , accountPasswordhistoryPassword = required "password"
    , accountPasswordhistoryTimestamp = required "timestamp"
    , accountPasswordhistoryUserId = required "user_id"
    }
  )

---- Types for table: account_signupcode ----

data AccountSignupcode' c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 =
  AccountSignupcode
    { accountSignupcodeId :: c1
    , accountSignupcodeCode :: c2
    , accountSignupcodeMaxUses :: c3
    , accountSignupcodeExpiry :: c4
    , accountSignupcodeInviterId :: c5
    , accountSignupcodeEmail :: c6
    , accountSignupcodeNotes :: c7
    , accountSignupcodeSent :: c8
    , accountSignupcodeCreated :: c9
    , accountSignupcodeUseCount :: c10
    }

type AccountSignupcode = AccountSignupcode' Int32 Text Int64 (Maybe LocalTime) (Maybe Int32) Text Text (Maybe LocalTime) LocalTime Int64

type AccountSignupcodeReadColumns = AccountSignupcode' (Column PGInt4) (Column PGText) (Column PGInt8) (Column (Nullable PGTimestamp)) (Column (Nullable PGInt4)) (Column PGText) (Column PGText) (Column (Nullable PGTimestamp)) (Column PGTimestamp) (Column PGInt8)

type AccountSignupcodeWriteColumns = AccountSignupcode' (Maybe (Column PGInt4)) (Column PGText) (Column PGInt8) (Maybe (Column (Nullable PGTimestamp))) (Maybe (Column (Nullable PGInt4))) (Column PGText) (Column PGText) (Maybe (Column (Nullable PGTimestamp))) (Column PGTimestamp) (Column PGInt8)

type AccountSignupcodeNullableColumns = AccountSignupcode' (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGInt8)) (Column (Nullable PGTimestamp)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGTimestamp)) (Column (Nullable PGTimestamp)) (Column (Nullable PGInt8))

type AccountSignupcodeNullable = AccountSignupcode' (Maybe Int32) (Maybe Text) (Maybe Int64) (Maybe LocalTime) (Maybe Int32) (Maybe Text) (Maybe Text) (Maybe LocalTime) (Maybe LocalTime) (Maybe Int64)

fromNullableAccountSignupcode :: AccountSignupcodeNullable -> Maybe AccountSignupcode
fromNullableAccountSignupcode = fromNullable

$(makeAdaptorAndInstance "pAccountSignupcode" ''AccountSignupcode')

accountSignupcodeTable :: Table AccountSignupcodeWriteColumns AccountSignupcodeReadColumns
accountSignupcodeTable = Table "account_signupcode" (pAccountSignupcode
  AccountSignupcode
    { accountSignupcodeId = optional "id"
    , accountSignupcodeCode = required "code"
    , accountSignupcodeMaxUses = required "max_uses"
    , accountSignupcodeExpiry = optional "expiry"
    , accountSignupcodeInviterId = optional "inviter_id"
    , accountSignupcodeEmail = required "email"
    , accountSignupcodeNotes = required "notes"
    , accountSignupcodeSent = optional "sent"
    , accountSignupcodeCreated = required "created"
    , accountSignupcodeUseCount = required "use_count"
    }
  )

---- Types for table: account_signupcoderesult ----

data AccountSignupcoderesult' c1 c2 c3 c4 =
  AccountSignupcoderesult
    { accountSignupcoderesultId :: c1
    , accountSignupcoderesultSignupCodeId :: c2
    , accountSignupcoderesultUserId :: c3
    , accountSignupcoderesultTimestamp :: c4
    }

type AccountSignupcoderesult = AccountSignupcoderesult' Int32 Int32 Int32 LocalTime

type AccountSignupcoderesultReadColumns = AccountSignupcoderesult' (Column PGInt4) (Column PGInt4) (Column PGInt4) (Column PGTimestamp)

type AccountSignupcoderesultWriteColumns = AccountSignupcoderesult' (Maybe (Column PGInt4)) (Column PGInt4) (Column PGInt4) (Column PGTimestamp)

type AccountSignupcoderesultNullableColumns = AccountSignupcoderesult' (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGTimestamp))

type AccountSignupcoderesultNullable = AccountSignupcoderesult' (Maybe Int32) (Maybe Int32) (Maybe Int32) (Maybe LocalTime)

fromNullableAccountSignupcoderesult :: AccountSignupcoderesultNullable -> Maybe AccountSignupcoderesult
fromNullableAccountSignupcoderesult = fromNullable

$(makeAdaptorAndInstance "pAccountSignupcoderesult" ''AccountSignupcoderesult')

accountSignupcoderesultTable :: Table AccountSignupcoderesultWriteColumns AccountSignupcoderesultReadColumns
accountSignupcoderesultTable = Table "account_signupcoderesult" (pAccountSignupcoderesult
  AccountSignupcoderesult
    { accountSignupcoderesultId = optional "id"
    , accountSignupcoderesultSignupCodeId = required "signup_code_id"
    , accountSignupcoderesultUserId = required "user_id"
    , accountSignupcoderesultTimestamp = required "timestamp"
    }
  )

---- Types for table: auth_group ----

data AuthGroup' c1 c2 =
  AuthGroup
    { authGroupId :: c1
    , authGroupName :: c2
    }

type AuthGroup = AuthGroup' Int32 Text

type AuthGroupReadColumns = AuthGroup' (Column PGInt4) (Column PGText)

type AuthGroupWriteColumns = AuthGroup' (Maybe (Column PGInt4)) (Column PGText)

type AuthGroupNullableColumns = AuthGroup' (Column (Nullable PGInt4)) (Column (Nullable PGText))

type AuthGroupNullable = AuthGroup' (Maybe Int32) (Maybe Text)

fromNullableAuthGroup :: AuthGroupNullable -> Maybe AuthGroup
fromNullableAuthGroup = fromNullable

$(makeAdaptorAndInstance "pAuthGroup" ''AuthGroup')

authGroupTable :: Table AuthGroupWriteColumns AuthGroupReadColumns
authGroupTable = Table "auth_group" (pAuthGroup
  AuthGroup
    { authGroupId = optional "id"
    , authGroupName = required "name"
    }
  )

---- Types for table: auth_group_permissions ----

data AuthGroupPermission' c1 c2 c3 =
  AuthGroupPermission
    { authGroupPermissionId :: c1
    , authGroupPermissionGroupId :: c2
    , authGroupPermissionPermissionId :: c3
    }

type AuthGroupPermission = AuthGroupPermission' Int32 Int32 Int32

type AuthGroupPermissionReadColumns = AuthGroupPermission' (Column PGInt4) (Column PGInt4) (Column PGInt4)

type AuthGroupPermissionWriteColumns = AuthGroupPermission' (Maybe (Column PGInt4)) (Column PGInt4) (Column PGInt4)

type AuthGroupPermissionNullableColumns = AuthGroupPermission' (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4))

type AuthGroupPermissionNullable = AuthGroupPermission' (Maybe Int32) (Maybe Int32) (Maybe Int32)

fromNullableAuthGroupPermission :: AuthGroupPermissionNullable -> Maybe AuthGroupPermission
fromNullableAuthGroupPermission = fromNullable

$(makeAdaptorAndInstance "pAuthGroupPermission" ''AuthGroupPermission')

authGroupPermissionTable :: Table AuthGroupPermissionWriteColumns AuthGroupPermissionReadColumns
authGroupPermissionTable = Table "auth_group_permissions" (pAuthGroupPermission
  AuthGroupPermission
    { authGroupPermissionId = optional "id"
    , authGroupPermissionGroupId = required "group_id"
    , authGroupPermissionPermissionId = required "permission_id"
    }
  )

---- Types for table: auth_permission ----

data AuthPermission' c1 c2 c3 c4 =
  AuthPermission
    { authPermissionId :: c1
    , authPermissionName :: c2
    , authPermissionContentTypeId :: c3
    , authPermissionCodename :: c4
    }

type AuthPermission = AuthPermission' Int32 Text Int32 Text

type AuthPermissionReadColumns = AuthPermission' (Column PGInt4) (Column PGText) (Column PGInt4) (Column PGText)

type AuthPermissionWriteColumns = AuthPermission' (Maybe (Column PGInt4)) (Column PGText) (Column PGInt4) (Column PGText)

type AuthPermissionNullableColumns = AuthPermission' (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGText))

type AuthPermissionNullable = AuthPermission' (Maybe Int32) (Maybe Text) (Maybe Int32) (Maybe Text)

fromNullableAuthPermission :: AuthPermissionNullable -> Maybe AuthPermission
fromNullableAuthPermission = fromNullable

$(makeAdaptorAndInstance "pAuthPermission" ''AuthPermission')

authPermissionTable :: Table AuthPermissionWriteColumns AuthPermissionReadColumns
authPermissionTable = Table "auth_permission" (pAuthPermission
  AuthPermission
    { authPermissionId = optional "id"
    , authPermissionName = required "name"
    , authPermissionContentTypeId = required "content_type_id"
    , authPermissionCodename = required "codename"
    }
  )

---- Types for table: auth_user ----

data AuthUser' c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 =
  AuthUser
    { authUserId :: c1
    , authUserPassword :: c2
    , authUserLastLogin :: c3
    , authUserIsSuperuser :: c4
    , authUserUsername :: c5
    , authUserFirstName :: c6
    , authUserLastName :: c7
    , authUserEmail :: c8
    , authUserIsStaff :: c9
    , authUserIsActive :: c10
    , authUserDateJoined :: c11
    }

type AuthUser = AuthUser' Int32 Text (Maybe LocalTime) Bool Text Text Text Text Bool Bool LocalTime

type AuthUserReadColumns = AuthUser' (Column PGInt4) (Column PGText) (Column (Nullable PGTimestamp)) (Column PGBool) (Column PGText) (Column PGText) (Column PGText) (Column PGText) (Column PGBool) (Column PGBool) (Column PGTimestamp)

type AuthUserWriteColumns = AuthUser' (Maybe (Column PGInt4)) (Column PGText) (Maybe (Column (Nullable PGTimestamp))) (Column PGBool) (Column PGText) (Column PGText) (Column PGText) (Column PGText) (Column PGBool) (Column PGBool) (Column PGTimestamp)

type AuthUserNullableColumns = AuthUser' (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGTimestamp)) (Column (Nullable PGBool)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGBool)) (Column (Nullable PGBool)) (Column (Nullable PGTimestamp))

type AuthUserNullable = AuthUser' (Maybe Int32) (Maybe Text) (Maybe LocalTime) (Maybe Bool) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Bool) (Maybe Bool) (Maybe LocalTime)

fromNullableAuthUser :: AuthUserNullable -> Maybe AuthUser
fromNullableAuthUser = fromNullable

$(makeAdaptorAndInstance "pAuthUser" ''AuthUser')

authUserTable :: Table AuthUserWriteColumns AuthUserReadColumns
authUserTable = Table "auth_user" (pAuthUser
  AuthUser
    { authUserId = optional "id"
    , authUserPassword = required "password"
    , authUserLastLogin = optional "last_login"
    , authUserIsSuperuser = required "is_superuser"
    , authUserUsername = required "username"
    , authUserFirstName = required "first_name"
    , authUserLastName = required "last_name"
    , authUserEmail = required "email"
    , authUserIsStaff = required "is_staff"
    , authUserIsActive = required "is_active"
    , authUserDateJoined = required "date_joined"
    }
  )

---- Types for table: auth_user_groups ----

data AuthUserGroup' c1 c2 c3 =
  AuthUserGroup
    { authUserGroupId :: c1
    , authUserGroupUserId :: c2
    , authUserGroupGroupId :: c3
    }

type AuthUserGroup = AuthUserGroup' Int32 Int32 Int32

type AuthUserGroupReadColumns = AuthUserGroup' (Column PGInt4) (Column PGInt4) (Column PGInt4)

type AuthUserGroupWriteColumns = AuthUserGroup' (Maybe (Column PGInt4)) (Column PGInt4) (Column PGInt4)

type AuthUserGroupNullableColumns = AuthUserGroup' (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4))

type AuthUserGroupNullable = AuthUserGroup' (Maybe Int32) (Maybe Int32) (Maybe Int32)

fromNullableAuthUserGroup :: AuthUserGroupNullable -> Maybe AuthUserGroup
fromNullableAuthUserGroup = fromNullable

$(makeAdaptorAndInstance "pAuthUserGroup" ''AuthUserGroup')

authUserGroupTable :: Table AuthUserGroupWriteColumns AuthUserGroupReadColumns
authUserGroupTable = Table "auth_user_groups" (pAuthUserGroup
  AuthUserGroup
    { authUserGroupId = optional "id"
    , authUserGroupUserId = required "user_id"
    , authUserGroupGroupId = required "group_id"
    }
  )

---- Types for table: auth_user_user_permissions ----

data AuthUserUserPermission' c1 c2 c3 =
  AuthUserUserPermission
    { authUserUserPermissionId :: c1
    , authUserUserPermissionUserId :: c2
    , authUserUserPermissionPermissionId :: c3
    }

type AuthUserUserPermission = AuthUserUserPermission' Int32 Int32 Int32

type AuthUserUserPermissionReadColumns = AuthUserUserPermission' (Column PGInt4) (Column PGInt4) (Column PGInt4)

type AuthUserUserPermissionWriteColumns = AuthUserUserPermission' (Maybe (Column PGInt4)) (Column PGInt4) (Column PGInt4)

type AuthUserUserPermissionNullableColumns = AuthUserUserPermission' (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4))

type AuthUserUserPermissionNullable = AuthUserUserPermission' (Maybe Int32) (Maybe Int32) (Maybe Int32)

fromNullableAuthUserUserPermission :: AuthUserUserPermissionNullable -> Maybe AuthUserUserPermission
fromNullableAuthUserUserPermission = fromNullable

$(makeAdaptorAndInstance "pAuthUserUserPermission" ''AuthUserUserPermission')

authUserUserPermissionTable :: Table AuthUserUserPermissionWriteColumns AuthUserUserPermissionReadColumns
authUserUserPermissionTable = Table "auth_user_user_permissions" (pAuthUserUserPermission
  AuthUserUserPermission
    { authUserUserPermissionId = optional "id"
    , authUserUserPermissionUserId = required "user_id"
    , authUserUserPermissionPermissionId = required "permission_id"
    }
  )

---- Types for table: authtoken_token ----

data AuthtokenToken' c1 c2 c3 =
  AuthtokenToken
    { authtokenTokenKey :: c1
    , authtokenTokenCreated :: c2
    , authtokenTokenUserId :: c3
    }

type AuthtokenToken = AuthtokenToken' Text UTCTime Int32

type AuthtokenTokenReadColumns = AuthtokenToken' (Column PGText) (Column PGTimestamptz) (Column PGInt4)

type AuthtokenTokenWriteColumns = AuthtokenToken' (Column PGText) (Column PGTimestamptz) (Column PGInt4)

type AuthtokenTokenNullableColumns = AuthtokenToken' (Column (Nullable PGText)) (Column (Nullable PGTimestamptz)) (Column (Nullable PGInt4))

type AuthtokenTokenNullable = AuthtokenToken' (Maybe Text) (Maybe UTCTime) (Maybe Int32)

fromNullableAuthtokenToken :: AuthtokenTokenNullable -> Maybe AuthtokenToken
fromNullableAuthtokenToken = fromNullable

$(makeAdaptorAndInstance "pAuthtokenToken" ''AuthtokenToken')

authtokenTokenTable :: Table AuthtokenTokenWriteColumns AuthtokenTokenReadColumns
authtokenTokenTable = Table "authtoken_token" (pAuthtokenToken
  AuthtokenToken
    { authtokenTokenKey = required "key"
    , authtokenTokenCreated = required "created"
    , authtokenTokenUserId = required "user_id"
    }
  )

---- Types for table: chat_message ----

data ChatMessage' c1 c2 c3 c4 c5 =
  ChatMessage
    { chatMessageId :: c1
    , chatMessageHandle :: c2
    , chatMessageMessage :: c3
    , chatMessageTimestamp :: c4
    , chatMessageRoomId :: c5
    }

type ChatMessage = ChatMessage' Int32 Text Text UTCTime Int32

type ChatMessageReadColumns = ChatMessage' (Column PGInt4) (Column PGText) (Column PGText) (Column PGTimestamptz) (Column PGInt4)

type ChatMessageWriteColumns = ChatMessage' (Maybe (Column PGInt4)) (Column PGText) (Column PGText) (Column PGTimestamptz) (Column PGInt4)

type ChatMessageNullableColumns = ChatMessage' (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGTimestamptz)) (Column (Nullable PGInt4))

type ChatMessageNullable = ChatMessage' (Maybe Int32) (Maybe Text) (Maybe Text) (Maybe UTCTime) (Maybe Int32)

fromNullableChatMessage :: ChatMessageNullable -> Maybe ChatMessage
fromNullableChatMessage = fromNullable

$(makeAdaptorAndInstance "pChatMessage" ''ChatMessage')

chatMessageTable :: Table ChatMessageWriteColumns ChatMessageReadColumns
chatMessageTable = Table "chat_message" (pChatMessage
  ChatMessage
    { chatMessageId = optional "id"
    , chatMessageHandle = required "handle"
    , chatMessageMessage = required "message"
    , chatMessageTimestamp = required "timestamp"
    , chatMessageRoomId = required "room_id"
    }
  )

---- Types for table: chat_room ----

data ChatRoom' c1 c2 c3 =
  ChatRoom
    { chatRoomId :: c1
    , chatRoomName :: c2
    , chatRoomLabel :: c3
    }

type ChatRoom = ChatRoom' Int32 Text Text

type ChatRoomReadColumns = ChatRoom' (Column PGInt4) (Column PGText) (Column PGText)

type ChatRoomWriteColumns = ChatRoom' (Maybe (Column PGInt4)) (Column PGText) (Column PGText)

type ChatRoomNullableColumns = ChatRoom' (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText))

type ChatRoomNullable = ChatRoom' (Maybe Int32) (Maybe Text) (Maybe Text)

fromNullableChatRoom :: ChatRoomNullable -> Maybe ChatRoom
fromNullableChatRoom = fromNullable

$(makeAdaptorAndInstance "pChatRoom" ''ChatRoom')

chatRoomTable :: Table ChatRoomWriteColumns ChatRoomReadColumns
chatRoomTable = Table "chat_room" (pChatRoom
  ChatRoom
    { chatRoomId = optional "id"
    , chatRoomName = required "name"
    , chatRoomLabel = required "label"
    }
  )

---- Types for table: corsheaders_corsmodel ----

data CorsheadersCorsmodel' c1 c2 =
  CorsheadersCorsmodel
    { corsheadersCorsmodelId :: c1
    , corsheadersCorsmodelCors :: c2
    }

type CorsheadersCorsmodel = CorsheadersCorsmodel' Int32 Text

type CorsheadersCorsmodelReadColumns = CorsheadersCorsmodel' (Column PGInt4) (Column PGText)

type CorsheadersCorsmodelWriteColumns = CorsheadersCorsmodel' (Maybe (Column PGInt4)) (Column PGText)

type CorsheadersCorsmodelNullableColumns = CorsheadersCorsmodel' (Column (Nullable PGInt4)) (Column (Nullable PGText))

type CorsheadersCorsmodelNullable = CorsheadersCorsmodel' (Maybe Int32) (Maybe Text)

fromNullableCorsheadersCorsmodel :: CorsheadersCorsmodelNullable -> Maybe CorsheadersCorsmodel
fromNullableCorsheadersCorsmodel = fromNullable

$(makeAdaptorAndInstance "pCorsheadersCorsmodel" ''CorsheadersCorsmodel')

corsheadersCorsmodelTable :: Table CorsheadersCorsmodelWriteColumns CorsheadersCorsmodelReadColumns
corsheadersCorsmodelTable = Table "corsheaders_corsmodel" (pCorsheadersCorsmodel
  CorsheadersCorsmodel
    { corsheadersCorsmodelId = optional "id"
    , corsheadersCorsmodelCors = required "cors"
    }
  )

---- Types for table: django_admin_log ----

data DjangoAdminLog' c1 c2 c3 c4 c5 c6 c7 c8 =
  DjangoAdminLog
    { djangoAdminLogId :: c1
    , djangoAdminLogActionTime :: c2
    , djangoAdminLogUserId :: c3
    , djangoAdminLogContentTypeId :: c4
    , djangoAdminLogObjectId :: c5
    , djangoAdminLogObjectRepr :: c6
    , djangoAdminLogActionFlag :: c7
    , djangoAdminLogChangeMessage :: c8
    }

type DjangoAdminLog = DjangoAdminLog' Int32 LocalTime Int32 (Maybe Int32) (Maybe Text) Text Int32 Text

type DjangoAdminLogReadColumns = DjangoAdminLog' (Column PGInt4) (Column PGTimestamp) (Column PGInt4) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column PGText) (Column PGInt4) (Column PGText)

type DjangoAdminLogWriteColumns = DjangoAdminLog' (Maybe (Column PGInt4)) (Column PGTimestamp) (Column PGInt4) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGText))) (Column PGText) (Column PGInt4) (Column PGText)

type DjangoAdminLogNullableColumns = DjangoAdminLog' (Column (Nullable PGInt4)) (Column (Nullable PGTimestamp)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGText))

type DjangoAdminLogNullable = DjangoAdminLog' (Maybe Int32) (Maybe LocalTime) (Maybe Int32) (Maybe Int32) (Maybe Text) (Maybe Text) (Maybe Int32) (Maybe Text)

fromNullableDjangoAdminLog :: DjangoAdminLogNullable -> Maybe DjangoAdminLog
fromNullableDjangoAdminLog = fromNullable

$(makeAdaptorAndInstance "pDjangoAdminLog" ''DjangoAdminLog')

djangoAdminLogTable :: Table DjangoAdminLogWriteColumns DjangoAdminLogReadColumns
djangoAdminLogTable = Table "django_admin_log" (pDjangoAdminLog
  DjangoAdminLog
    { djangoAdminLogId = optional "id"
    , djangoAdminLogActionTime = required "action_time"
    , djangoAdminLogUserId = required "user_id"
    , djangoAdminLogContentTypeId = optional "content_type_id"
    , djangoAdminLogObjectId = optional "object_id"
    , djangoAdminLogObjectRepr = required "object_repr"
    , djangoAdminLogActionFlag = required "action_flag"
    , djangoAdminLogChangeMessage = required "change_message"
    }
  )

---- Types for table: django_content_type ----

data DjangoContentType' c1 c2 c3 =
  DjangoContentType
    { djangoContentTypeId :: c1
    , djangoContentTypeAppLabel :: c2
    , djangoContentTypeModel :: c3
    }

type DjangoContentType = DjangoContentType' Int32 Text Text

type DjangoContentTypeReadColumns = DjangoContentType' (Column PGInt4) (Column PGText) (Column PGText)

type DjangoContentTypeWriteColumns = DjangoContentType' (Maybe (Column PGInt4)) (Column PGText) (Column PGText)

type DjangoContentTypeNullableColumns = DjangoContentType' (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText))

type DjangoContentTypeNullable = DjangoContentType' (Maybe Int32) (Maybe Text) (Maybe Text)

fromNullableDjangoContentType :: DjangoContentTypeNullable -> Maybe DjangoContentType
fromNullableDjangoContentType = fromNullable

$(makeAdaptorAndInstance "pDjangoContentType" ''DjangoContentType')

djangoContentTypeTable :: Table DjangoContentTypeWriteColumns DjangoContentTypeReadColumns
djangoContentTypeTable = Table "django_content_type" (pDjangoContentType
  DjangoContentType
    { djangoContentTypeId = optional "id"
    , djangoContentTypeAppLabel = required "app_label"
    , djangoContentTypeModel = required "model"
    }
  )

---- Types for table: django_migrations ----

data DjangoMigration' c1 c2 c3 c4 =
  DjangoMigration
    { djangoMigrationId :: c1
    , djangoMigrationApp :: c2
    , djangoMigrationName :: c3
    , djangoMigrationApplied :: c4
    }

type DjangoMigration = DjangoMigration' Int32 Text Text LocalTime

type DjangoMigrationReadColumns = DjangoMigration' (Column PGInt4) (Column PGText) (Column PGText) (Column PGTimestamp)

type DjangoMigrationWriteColumns = DjangoMigration' (Maybe (Column PGInt4)) (Column PGText) (Column PGText) (Column PGTimestamp)

type DjangoMigrationNullableColumns = DjangoMigration' (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGTimestamp))

type DjangoMigrationNullable = DjangoMigration' (Maybe Int32) (Maybe Text) (Maybe Text) (Maybe LocalTime)

fromNullableDjangoMigration :: DjangoMigrationNullable -> Maybe DjangoMigration
fromNullableDjangoMigration = fromNullable

$(makeAdaptorAndInstance "pDjangoMigration" ''DjangoMigration')

djangoMigrationTable :: Table DjangoMigrationWriteColumns DjangoMigrationReadColumns
djangoMigrationTable = Table "django_migrations" (pDjangoMigration
  DjangoMigration
    { djangoMigrationId = optional "id"
    , djangoMigrationApp = required "app"
    , djangoMigrationName = required "name"
    , djangoMigrationApplied = required "applied"
    }
  )

---- Types for table: django_session ----

data DjangoSession' c1 c2 c3 =
  DjangoSession
    { djangoSessionSessionKey :: c1
    , djangoSessionSessionData :: c2
    , djangoSessionExpireDate :: c3
    }

type DjangoSession = DjangoSession' Text Text LocalTime

type DjangoSessionReadColumns = DjangoSession' (Column PGText) (Column PGText) (Column PGTimestamp)

type DjangoSessionWriteColumns = DjangoSession' (Column PGText) (Column PGText) (Column PGTimestamp)

type DjangoSessionNullableColumns = DjangoSession' (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGTimestamp))

type DjangoSessionNullable = DjangoSession' (Maybe Text) (Maybe Text) (Maybe LocalTime)

fromNullableDjangoSession :: DjangoSessionNullable -> Maybe DjangoSession
fromNullableDjangoSession = fromNullable

$(makeAdaptorAndInstance "pDjangoSession" ''DjangoSession')

djangoSessionTable :: Table DjangoSessionWriteColumns DjangoSessionReadColumns
djangoSessionTable = Table "django_session" (pDjangoSession
  DjangoSession
    { djangoSessionSessionKey = required "session_key"
    , djangoSessionSessionData = required "session_data"
    , djangoSessionExpireDate = required "expire_date"
    }
  )

---- Types for table: django_site ----

data DjangoSite' c1 c2 c3 =
  DjangoSite
    { djangoSiteId :: c1
    , djangoSiteDomain :: c2
    , djangoSiteName :: c3
    }

type DjangoSite = DjangoSite' Int32 Text Text

type DjangoSiteReadColumns = DjangoSite' (Column PGInt4) (Column PGText) (Column PGText)

type DjangoSiteWriteColumns = DjangoSite' (Maybe (Column PGInt4)) (Column PGText) (Column PGText)

type DjangoSiteNullableColumns = DjangoSite' (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText))

type DjangoSiteNullable = DjangoSite' (Maybe Int32) (Maybe Text) (Maybe Text)

fromNullableDjangoSite :: DjangoSiteNullable -> Maybe DjangoSite
fromNullableDjangoSite = fromNullable

$(makeAdaptorAndInstance "pDjangoSite" ''DjangoSite')

djangoSiteTable :: Table DjangoSiteWriteColumns DjangoSiteReadColumns
djangoSiteTable = Table "django_site" (pDjangoSite
  DjangoSite
    { djangoSiteId = optional "id"
    , djangoSiteDomain = required "domain"
    , djangoSiteName = required "name"
    }
  )

---- Types for table: eventlog_log ----

data EventlogLog' c1 c2 c3 c4 c5 =
  EventlogLog
    { eventlogLogId :: c1
    , eventlogLogUserId :: c2
    , eventlogLogTimestamp :: c3
    , eventlogLogAction :: c4
    , eventlogLogExtra :: c5
    }

type EventlogLog = EventlogLog' Int32 (Maybe Int32) LocalTime Text Text

type EventlogLogReadColumns = EventlogLog' (Column PGInt4) (Column (Nullable PGInt4)) (Column PGTimestamp) (Column PGText) (Column PGText)

type EventlogLogWriteColumns = EventlogLog' (Maybe (Column PGInt4)) (Maybe (Column (Nullable PGInt4))) (Column PGTimestamp) (Column PGText) (Column PGText)

type EventlogLogNullableColumns = EventlogLog' (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGTimestamp)) (Column (Nullable PGText)) (Column (Nullable PGText))

type EventlogLogNullable = EventlogLog' (Maybe Int32) (Maybe Int32) (Maybe LocalTime) (Maybe Text) (Maybe Text)

fromNullableEventlogLog :: EventlogLogNullable -> Maybe EventlogLog
fromNullableEventlogLog = fromNullable

$(makeAdaptorAndInstance "pEventlogLog" ''EventlogLog')

eventlogLogTable :: Table EventlogLogWriteColumns EventlogLogReadColumns
eventlogLogTable = Table "eventlog_log" (pEventlogLog
  EventlogLog
    { eventlogLogId = optional "id"
    , eventlogLogUserId = optional "user_id"
    , eventlogLogTimestamp = required "timestamp"
    , eventlogLogAction = required "action"
    , eventlogLogExtra = required "extra"
    }
  )

---- Types for table: south_migrationhistory ----

data SouthMigrationhistory' c1 c2 c3 c4 =
  SouthMigrationhistory
    { southMigrationhistoryId :: c1
    , southMigrationhistoryAppName :: c2
    , southMigrationhistoryMigration :: c3
    , southMigrationhistoryApplied :: c4
    }

type SouthMigrationhistory = SouthMigrationhistory' Int32 Text Text LocalTime

type SouthMigrationhistoryReadColumns = SouthMigrationhistory' (Column PGInt4) (Column PGText) (Column PGText) (Column PGTimestamp)

type SouthMigrationhistoryWriteColumns = SouthMigrationhistory' (Maybe (Column PGInt4)) (Column PGText) (Column PGText) (Column PGTimestamp)

type SouthMigrationhistoryNullableColumns = SouthMigrationhistory' (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGTimestamp))

type SouthMigrationhistoryNullable = SouthMigrationhistory' (Maybe Int32) (Maybe Text) (Maybe Text) (Maybe LocalTime)

fromNullableSouthMigrationhistory :: SouthMigrationhistoryNullable -> Maybe SouthMigrationhistory
fromNullableSouthMigrationhistory = fromNullable

$(makeAdaptorAndInstance "pSouthMigrationhistory" ''SouthMigrationhistory')

southMigrationhistoryTable :: Table SouthMigrationhistoryWriteColumns SouthMigrationhistoryReadColumns
southMigrationhistoryTable = Table "south_migrationhistory" (pSouthMigrationhistory
  SouthMigrationhistory
    { southMigrationhistoryId = optional "id"
    , southMigrationhistoryAppName = required "app_name"
    , southMigrationhistoryMigration = required "migration"
    , southMigrationhistoryApplied = required "applied"
    }
  )

---- Types for table: tennis_courttime ----

data TennisCourttime' c1 c2 c3 c4 c5 c6 =
  TennisCourttime
    { tennisCourttimeId :: c1
    , tennisCourttimeLocationId :: c2
    , tennisCourttimeCourtNo :: c3
    , tennisCourttimeCourtDate :: c4
    , tennisCourttimeCourtTime :: c5
    , tennisCourttimeReservedById :: c6
    }

type TennisCourttime = TennisCourttime' Int32 Int32 Text Day TimeOfDay (Maybe Int32)

type TennisCourttimeReadColumns = TennisCourttime' (Column PGInt4) (Column PGInt4) (Column PGText) (Column PGDate) (Column PGTime) (Column (Nullable PGInt4))

type TennisCourttimeWriteColumns = TennisCourttime' (Maybe (Column PGInt4)) (Column PGInt4) (Column PGText) (Column PGDate) (Column PGTime) (Maybe (Column (Nullable PGInt4)))

type TennisCourttimeNullableColumns = TennisCourttime' (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGDate)) (Column (Nullable PGTime)) (Column (Nullable PGInt4))

type TennisCourttimeNullable = TennisCourttime' (Maybe Int32) (Maybe Int32) (Maybe Text) (Maybe Day) (Maybe TimeOfDay) (Maybe Int32)

fromNullableTennisCourttime :: TennisCourttimeNullable -> Maybe TennisCourttime
fromNullableTennisCourttime = fromNullable

$(makeAdaptorAndInstance "pTennisCourttime" ''TennisCourttime')

tennisCourttimeTable :: Table TennisCourttimeWriteColumns TennisCourttimeReadColumns
tennisCourttimeTable = Table "tennis_courttime" (pTennisCourttime
  TennisCourttime
    { tennisCourttimeId = optional "id"
    , tennisCourttimeLocationId = required "location_id"
    , tennisCourttimeCourtNo = required "court_no"
    , tennisCourttimeCourtDate = required "court_date"
    , tennisCourttimeCourtTime = required "court_time"
    , tennisCourttimeReservedById = optional "reserved_by_id"
    }
  )

---- Types for table: tennis_event ----

data TennisEvent' c1 c2 c3 c4 c5 c6 c7 c8 =
  TennisEvent
    { tennisEventId :: c1
    , tennisEventDate :: c2
    , tennisEventName :: c3
    , tennisEventOrgId :: c4
    , tennisEventEventType :: c5
    , tennisEventComment :: c6
    , tennisEventAlwaysShow :: c7
    , tennisEventLeagueId :: c8
    }

type TennisEvent = TennisEvent' Int32 LocalTime Text Int32 (Maybe Text) (Maybe Text) Bool (Maybe Int32)

type TennisEventReadColumns = TennisEvent' (Column PGInt4) (Column PGTimestamp) (Column PGText) (Column PGInt4) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column PGBool) (Column (Nullable PGInt4))

type TennisEventWriteColumns = TennisEvent' (Maybe (Column PGInt4)) (Column PGTimestamp) (Column PGText) (Column PGInt4) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Column PGBool) (Maybe (Column (Nullable PGInt4)))

type TennisEventNullableColumns = TennisEvent' (Column (Nullable PGInt4)) (Column (Nullable PGTimestamp)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGBool)) (Column (Nullable PGInt4))

type TennisEventNullable = TennisEvent' (Maybe Int32) (Maybe LocalTime) (Maybe Text) (Maybe Int32) (Maybe Text) (Maybe Text) (Maybe Bool) (Maybe Int32)

fromNullableTennisEvent :: TennisEventNullable -> Maybe TennisEvent
fromNullableTennisEvent = fromNullable

$(makeAdaptorAndInstance "pTennisEvent" ''TennisEvent')

tennisEventTable :: Table TennisEventWriteColumns TennisEventReadColumns
tennisEventTable = Table "tennis_event" (pTennisEvent
  TennisEvent
    { tennisEventId = optional "id"
    , tennisEventDate = required "date"
    , tennisEventName = required "name"
    , tennisEventOrgId = required "org_id"
    , tennisEventEventType = optional "event_type"
    , tennisEventComment = optional "comment"
    , tennisEventAlwaysShow = required "always_show"
    , tennisEventLeagueId = optional "league_id"
    }
  )

---- Types for table: tennis_eventrsvp ----

data TennisEventrsvp' c1 c2 c3 c4 c5 =
  TennisEventrsvp
    { tennisEventrsvpId :: c1
    , tennisEventrsvpEventId :: c2
    , tennisEventrsvpPlayerId :: c3
    , tennisEventrsvpResponse :: c4
    , tennisEventrsvpComment :: c5
    }

type TennisEventrsvp = TennisEventrsvp' Int32 Int32 Int32 Text Text

type TennisEventrsvpReadColumns = TennisEventrsvp' (Column PGInt4) (Column PGInt4) (Column PGInt4) (Column PGText) (Column PGText)

type TennisEventrsvpWriteColumns = TennisEventrsvp' (Maybe (Column PGInt4)) (Column PGInt4) (Column PGInt4) (Column PGText) (Column PGText)

type TennisEventrsvpNullableColumns = TennisEventrsvp' (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText))

type TennisEventrsvpNullable = TennisEventrsvp' (Maybe Int32) (Maybe Int32) (Maybe Int32) (Maybe Text) (Maybe Text)

fromNullableTennisEventrsvp :: TennisEventrsvpNullable -> Maybe TennisEventrsvp
fromNullableTennisEventrsvp = fromNullable

$(makeAdaptorAndInstance "pTennisEventrsvp" ''TennisEventrsvp')

tennisEventrsvpTable :: Table TennisEventrsvpWriteColumns TennisEventrsvpReadColumns
tennisEventrsvpTable = Table "tennis_eventrsvp" (pTennisEventrsvp
  TennisEventrsvp
    { tennisEventrsvpId = optional "id"
    , tennisEventrsvpEventId = required "event_id"
    , tennisEventrsvpPlayerId = required "player_id"
    , tennisEventrsvpResponse = required "response"
    , tennisEventrsvpComment = required "comment"
    }
  )

---- Types for table: tennis_facility ----

data TennisFacility' c1 c2 c3 c4 c5 c6 =
  TennisFacility
    { tennisFacilityId :: c1
    , tennisFacilityName :: c2
    , tennisFacilityAbbrev :: c3
    , tennisFacilityPhone :: c4
    , tennisFacilityAddress :: c5
    , tennisFacilityMapUrl :: c6
    }

type TennisFacility = TennisFacility' Int32 Text (Maybe Text) (Maybe Text) (Maybe Text) Text

type TennisFacilityReadColumns = TennisFacility' (Column PGInt4) (Column PGText) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column PGText)

type TennisFacilityWriteColumns = TennisFacility' (Maybe (Column PGInt4)) (Column PGText) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Column PGText)

type TennisFacilityNullableColumns = TennisFacility' (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText))

type TennisFacilityNullable = TennisFacility' (Maybe Int32) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text)

fromNullableTennisFacility :: TennisFacilityNullable -> Maybe TennisFacility
fromNullableTennisFacility = fromNullable

$(makeAdaptorAndInstance "pTennisFacility" ''TennisFacility')

tennisFacilityTable :: Table TennisFacilityWriteColumns TennisFacilityReadColumns
tennisFacilityTable = Table "tennis_facility" (pTennisFacility
  TennisFacility
    { tennisFacilityId = optional "id"
    , tennisFacilityName = required "name"
    , tennisFacilityAbbrev = optional "abbrev"
    , tennisFacilityPhone = optional "phone"
    , tennisFacilityAddress = optional "address"
    , tennisFacilityMapUrl = required "map_url"
    }
  )

---- Types for table: tennis_league ----

data TennisLeague' c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 =
  TennisLeague
    { tennisLeagueId :: c1
    , tennisLeagueName :: c2
    , tennisLeagueAbbrev :: c3
    , tennisLeagueOrgId :: c4
    , tennisLeagueYear :: c5
    , tennisLeagueSeason :: c6
    , tennisLeagueDivision :: c7
    , tennisLeagueGroup :: c8
    , tennisLeagueBeginDate :: c9
    , tennisLeagueIsKnockout :: c10
    }

type TennisLeague = TennisLeague' Int32 Text (Maybe Text) Int32 (Maybe Int32) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Day) (Maybe Bool)

type TennisLeagueReadColumns = TennisLeague' (Column PGInt4) (Column PGText) (Column (Nullable PGText)) (Column PGInt4) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGDate)) (Column (Nullable PGBool))

type TennisLeagueWriteColumns = TennisLeague' (Maybe (Column PGInt4)) (Column PGText) (Maybe (Column (Nullable PGText))) (Column PGInt4) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGDate))) (Maybe (Column (Nullable PGBool)))

type TennisLeagueNullableColumns = TennisLeague' (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGDate)) (Column (Nullable PGBool))

type TennisLeagueNullable = TennisLeague' (Maybe Int32) (Maybe Text) (Maybe Text) (Maybe Int32) (Maybe Int32) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Day) (Maybe Bool)

fromNullableTennisLeague :: TennisLeagueNullable -> Maybe TennisLeague
fromNullableTennisLeague = fromNullable

$(makeAdaptorAndInstance "pTennisLeague" ''TennisLeague')

tennisLeagueTable :: Table TennisLeagueWriteColumns TennisLeagueReadColumns
tennisLeagueTable = Table "tennis_league" (pTennisLeague
  TennisLeague
    { tennisLeagueId = optional "id"
    , tennisLeagueName = required "name"
    , tennisLeagueAbbrev = optional "abbrev"
    , tennisLeagueOrgId = required "org_id"
    , tennisLeagueYear = optional "year"
    , tennisLeagueSeason = optional "season"
    , tennisLeagueDivision = optional "division"
    , tennisLeagueGroup = optional "group"
    , tennisLeagueBeginDate = optional "begin_date"
    , tennisLeagueIsKnockout = optional "is_knockout"
    }
  )

---- Types for table: tennis_match ----

data TennisMatch' c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 =
  TennisMatch
    { tennisMatchId :: c1
    , tennisMatchDate :: c2
    , tennisMatchLeagueId :: c3
    , tennisMatchHomePlayer1Id :: c4
    , tennisMatchHomePlayer2Id :: c5
    , tennisMatchAwayPlayer1Id :: c6
    , tennisMatchAwayPlayer2Id :: c7
    , tennisMatchHomeWon :: c8
    , tennisMatchScore :: c9
    , tennisMatchComment :: c10
    , tennisMatchRoundNum :: c11
    , tennisMatchMatchNum :: c12
    }

type TennisMatch = TennisMatch' Int32 Day Int32 Int32 (Maybe Int32) Int32 (Maybe Int32) (Maybe Bool) (Maybe Text) Text (Maybe Int32) (Maybe Int32)

type TennisMatchReadColumns = TennisMatch' (Column PGInt4) (Column PGDate) (Column PGInt4) (Column PGInt4) (Column (Nullable PGInt4)) (Column PGInt4) (Column (Nullable PGInt4)) (Column (Nullable PGBool)) (Column (Nullable PGText)) (Column PGText) (Column (Nullable PGInt4)) (Column (Nullable PGInt4))

type TennisMatchWriteColumns = TennisMatch' (Maybe (Column PGInt4)) (Column PGDate) (Column PGInt4) (Column PGInt4) (Maybe (Column (Nullable PGInt4))) (Column PGInt4) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGBool))) (Maybe (Column (Nullable PGText))) (Column PGText) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4)))

type TennisMatchNullableColumns = TennisMatch' (Column (Nullable PGInt4)) (Column (Nullable PGDate)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGBool)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4))

type TennisMatchNullable = TennisMatch' (Maybe Int32) (Maybe Day) (Maybe Int32) (Maybe Int32) (Maybe Int32) (Maybe Int32) (Maybe Int32) (Maybe Bool) (Maybe Text) (Maybe Text) (Maybe Int32) (Maybe Int32)

fromNullableTennisMatch :: TennisMatchNullable -> Maybe TennisMatch
fromNullableTennisMatch = fromNullable

$(makeAdaptorAndInstance "pTennisMatch" ''TennisMatch')

tennisMatchTable :: Table TennisMatchWriteColumns TennisMatchReadColumns
tennisMatchTable = Table "tennis_match" (pTennisMatch
  TennisMatch
    { tennisMatchId = optional "id"
    , tennisMatchDate = required "date"
    , tennisMatchLeagueId = required "league_id"
    , tennisMatchHomePlayer1Id = required "home_player1_id"
    , tennisMatchHomePlayer2Id = optional "home_player2_id"
    , tennisMatchAwayPlayer1Id = required "away_player1_id"
    , tennisMatchAwayPlayer2Id = optional "away_player2_id"
    , tennisMatchHomeWon = optional "home_won"
    , tennisMatchScore = optional "score"
    , tennisMatchComment = required "comment"
    , tennisMatchRoundNum = optional "round_num"
    , tennisMatchMatchNum = optional "match_num"
    }
  )

---- Types for table: tennis_organization ----

data TennisOrganization' c1 c2 c3 c4 c5 =
  TennisOrganization
    { tennisOrganizationId :: c1
    , tennisOrganizationName :: c2
    , tennisOrganizationAbbrev :: c3
    , tennisOrganizationAdministratorId :: c4
    , tennisOrganizationMembersGroupId :: c5
    }

type TennisOrganization = TennisOrganization' Int32 Text (Maybe Text) Int32 Int32

type TennisOrganizationReadColumns = TennisOrganization' (Column PGInt4) (Column PGText) (Column (Nullable PGText)) (Column PGInt4) (Column PGInt4)

type TennisOrganizationWriteColumns = TennisOrganization' (Maybe (Column PGInt4)) (Column PGText) (Maybe (Column (Nullable PGText))) (Column PGInt4) (Column PGInt4)

type TennisOrganizationNullableColumns = TennisOrganization' (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4))

type TennisOrganizationNullable = TennisOrganization' (Maybe Int32) (Maybe Text) (Maybe Text) (Maybe Int32) (Maybe Int32)

fromNullableTennisOrganization :: TennisOrganizationNullable -> Maybe TennisOrganization
fromNullableTennisOrganization = fromNullable

$(makeAdaptorAndInstance "pTennisOrganization" ''TennisOrganization')

tennisOrganizationTable :: Table TennisOrganizationWriteColumns TennisOrganizationReadColumns
tennisOrganizationTable = Table "tennis_organization" (pTennisOrganization
  TennisOrganization
    { tennisOrganizationId = optional "id"
    , tennisOrganizationName = required "name"
    , tennisOrganizationAbbrev = optional "abbrev"
    , tennisOrganizationAdministratorId = required "administrator_id"
    , tennisOrganizationMembersGroupId = required "members_group_id"
    }
  )

---- Types for table: tennis_player ----

data TennisPlayer' c1 c2 c3 c4 c5 c6 c7 =
  TennisPlayer
    { tennisPlayerId :: c1
    , tennisPlayerUserId :: c2
    , tennisPlayerMobilePhone :: c3
    , tennisPlayerWorkPhone :: c4
    , tennisPlayerHomePhone :: c5
    , tennisPlayerFormatPreference :: c6
    , tennisPlayerSocialUserId :: c7
    }

type TennisPlayer = TennisPlayer' Int32 Int32 (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text)

type TennisPlayerReadColumns = TennisPlayer' (Column PGInt4) (Column PGInt4) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText))

type TennisPlayerWriteColumns = TennisPlayer' (Maybe (Column PGInt4)) (Column PGInt4) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText))) (Maybe (Column (Nullable PGText)))

type TennisPlayerNullableColumns = TennisPlayer' (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText)) (Column (Nullable PGText))

type TennisPlayerNullable = TennisPlayer' (Maybe Int32) (Maybe Int32) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Text)

fromNullableTennisPlayer :: TennisPlayerNullable -> Maybe TennisPlayer
fromNullableTennisPlayer = fromNullable

$(makeAdaptorAndInstance "pTennisPlayer" ''TennisPlayer')

tennisPlayerTable :: Table TennisPlayerWriteColumns TennisPlayerReadColumns
tennisPlayerTable = Table "tennis_player" (pTennisPlayer
  TennisPlayer
    { tennisPlayerId = optional "id"
    , tennisPlayerUserId = required "user_id"
    , tennisPlayerMobilePhone = optional "mobile_phone"
    , tennisPlayerWorkPhone = optional "work_phone"
    , tennisPlayerHomePhone = optional "home_phone"
    , tennisPlayerFormatPreference = optional "format_preference"
    , tennisPlayerSocialUserId = optional "social_user_id"
    }
  )

---- Types for table: tennis_pprefs ----

data TennisPpref' c1 c2 c3 c4 c5 c6 =
  TennisPpref
    { tennisPprefId :: c1
    , tennisPprefRatingAdj :: c2
    , tennisPprefP1Id :: c3
    , tennisPprefP2Id :: c4
    , tennisPprefP3Id :: c5
    , tennisPprefPlayerId :: c6
    }

type TennisPpref = TennisPpref' Int32 Scientific (Maybe Int32) (Maybe Int32) (Maybe Int32) Int32

type TennisPprefReadColumns = TennisPpref' (Column PGInt4) (Column PGNumeric) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column PGInt4)

type TennisPprefWriteColumns = TennisPpref' (Maybe (Column PGInt4)) (Column PGNumeric) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4))) (Maybe (Column (Nullable PGInt4))) (Column PGInt4)

type TennisPprefNullableColumns = TennisPpref' (Column (Nullable PGInt4)) (Column (Nullable PGNumeric)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4))

type TennisPprefNullable = TennisPpref' (Maybe Int32) (Maybe Scientific) (Maybe Int32) (Maybe Int32) (Maybe Int32) (Maybe Int32)

fromNullableTennisPpref :: TennisPprefNullable -> Maybe TennisPpref
fromNullableTennisPpref = fromNullable

$(makeAdaptorAndInstance "pTennisPpref" ''TennisPpref')

tennisPprefTable :: Table TennisPprefWriteColumns TennisPprefReadColumns
tennisPprefTable = Table "tennis_pprefs" (pTennisPpref
  TennisPpref
    { tennisPprefId = optional "id"
    , tennisPprefRatingAdj = required "rating_adj"
    , tennisPprefP1Id = optional "p1_id"
    , tennisPprefP2Id = optional "p2_id"
    , tennisPprefP3Id = optional "p3_id"
    , tennisPprefPlayerId = required "player_id"
    }
  )

---- Types for table: tennis_registration ----

data TennisRegistration' c1 c2 c3 =
  TennisRegistration
    { tennisRegistrationId :: c1
    , tennisRegistrationLeagueId :: c2
    , tennisRegistrationPlayerId :: c3
    }

type TennisRegistration = TennisRegistration' Int32 Int32 Int32

type TennisRegistrationReadColumns = TennisRegistration' (Column PGInt4) (Column PGInt4) (Column PGInt4)

type TennisRegistrationWriteColumns = TennisRegistration' (Maybe (Column PGInt4)) (Column PGInt4) (Column PGInt4)

type TennisRegistrationNullableColumns = TennisRegistration' (Column (Nullable PGInt4)) (Column (Nullable PGInt4)) (Column (Nullable PGInt4))

type TennisRegistrationNullable = TennisRegistration' (Maybe Int32) (Maybe Int32) (Maybe Int32)

fromNullableTennisRegistration :: TennisRegistrationNullable -> Maybe TennisRegistration
fromNullableTennisRegistration = fromNullable

$(makeAdaptorAndInstance "pTennisRegistration" ''TennisRegistration')

tennisRegistrationTable :: Table TennisRegistrationWriteColumns TennisRegistrationReadColumns
tennisRegistrationTable = Table "tennis_registration" (pTennisRegistration
  TennisRegistration
    { tennisRegistrationId = optional "id"
    , tennisRegistrationLeagueId = required "league_id"
    , tennisRegistrationPlayerId = required "player_id"
    }
  )

