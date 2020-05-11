{-# LANGUAGE UndecidableInstances #-}


{-# OPTIONS_GHC -fno-warn-orphans #-} --TODO: Eliminate this
{-# OPTIONS_GHC -Wno-redundant-constraints #-} --TODO: Stop the TH in this module from generating lots of redundant constraints
module Common.Schema where

import Obelisk.Beam.DZippable
import Obelisk.Beam.Constraints
import Obelisk.Auth
import Obelisk.Auth.SecureChannel.Common.Schema
import Control.Lens
import Data.Aeson
import Data.Int
import Data.Text (Text)
import Database.Beam
import Database.Beam.Backend.SQL.Types
import Database.Beam.Backend.SQL
import Obelisk.Beam.Schema
import Data.GADT.Show.TH
import Data.GADT.Compare.TH
import Obelisk.Schema


--------------------------------------------------------------------------------
-- Db
--------------------------------------------------------------------------------

data Db f = Db
  { _db_messages :: f (TableEntity Message)
  , _db_users :: f (TableEntity User)
  , _db_channels :: f (TableEntity Channel)
  , _db_channelMembers :: f (TableEntity ChannelMember)
  , _db_emailAuth :: SecureChannelAuth (Column EmailAddress) f -- TODO: Express foreign key relationship
  }
  deriving (Generic)

instance Database be Db
instance DZippable Db

instance ArgDictT Db where
  type ConstraintsForT Db c =
    ( c (TableEntity Message)
    , c (TableEntity User)
    , c (TableEntity Channel)
    , c (TableEntity ChannelMember)
    , ConstraintsForT (SecureChannelAuth (Column EmailAddress)) c
    )
  hoistWithArgDictT_ (_ :: proxy c) f (Db a b c d e) = Db (f a) (f b) (f c) (f d) (hoistWithArgDictT @_ @c f e)

--------------------------------------------------------------------------------
-- Message
--------------------------------------------------------------------------------

data Message f = Message
  { _message_id :: Columnar f (SqlSerial Int64)
  , _message_body :: Columnar f Text
  , _message_sender :: PrimaryKey User f
  , _message_channel :: PrimaryKey Channel f
  }
  deriving (Generic)

instance Beamable Message

instance Table Message where
  newtype PrimaryKey Message f = MessageId { _messageId_id :: Columnar f (SqlSerial Int64) }
    deriving (Generic)
  primaryKey = MessageId . _message_id

instance Beamable (PrimaryKey Message)

-- User: see Obelisk.Auth

deriving instance (BeamSqlBackend syntax, HasSqlEqualityCheck syntax (Columnar f (SqlSerial Int64))) => HasSqlEqualityCheck syntax (PrimaryKey User f)
deriving instance HasSqlValueSyntax syntax (Columnar f (SqlSerial Int64)) => HasSqlValueSyntax syntax (PrimaryKey User f)

--------------------------------------------------------------------------------
-- Channel
--------------------------------------------------------------------------------

data Channel f = Channel
  { _channel_id :: Columnar f (SqlSerial Int64)
  , _channel_name :: Columnar f Text
  }
  deriving (Generic)

instance Beamable Channel

instance Table Channel where
  newtype PrimaryKey Channel f = ChannelId { _channelId_id :: Columnar f (SqlSerial Int64) }
    deriving (Generic)
  primaryKey = ChannelId . _channel_id

_ChannelId :: Iso (PrimaryKey Channel f) (PrimaryKey Channel g) (Columnar f (SqlSerial Int64)) (Columnar g (SqlSerial Int64))
_ChannelId = iso _channelId_id ChannelId

instance Beamable (PrimaryKey Channel)

--------------------------------------------------------------------------------
-- ChannelMember
--------------------------------------------------------------------------------

data ChannelMember f = ChannelMember
  { _channelMember_channel :: PrimaryKey Channel f
  , _channelMember_user :: PrimaryKey User f
  , _channelMember_isOperator :: Columnar f Bool -- only channel operator can delete channels
  }
  deriving (Generic)

instance Beamable ChannelMember

instance Table ChannelMember where
  data PrimaryKey ChannelMember f = ChannelMemberId
    { _channelMemberId_channel :: PrimaryKey Channel f
    , _channelMemberId_user :: PrimaryKey User f
    }
    deriving (Generic)
  primaryKey c = ChannelMemberId
    { _channelMemberId_channel = _channelMember_channel c
    , _channelMemberId_user = _channelMember_user c
    }

instance Beamable (PrimaryKey ChannelMember)

--------------------------------------------------------------------------------
-- TH deriving
--------------------------------------------------------------------------------

-- Db
makeLenses ''Db
deriveFields1 ''Db "DbField1"
deriveGShow ''DbField1
deriveGEq ''DbField1
deriveGCompare ''DbField1

deriveFields1 ''Message "MessageField1"
deriveGShow ''MessageField1
deriveGEq ''MessageField1
deriveGCompare ''MessageField1

deriveFields1 ''Channel "ChannelField1"
deriveGShow ''ChannelField1
deriveGEq ''ChannelField1
deriveGCompare ''ChannelField1

deriveFields1 'ChannelId "ChannelIdField1"
deriveGShow ''ChannelIdField1
deriveGEq ''ChannelIdField1
deriveGCompare ''ChannelIdField1

deriveFields1 ''ChannelMember "ChannelMemberField1"
deriveGShow ''ChannelMemberField1
deriveGEq ''ChannelMemberField1
deriveGCompare ''ChannelMemberField1

deriveFields1 'ChannelMemberId "ChannelMemberIdField1"
deriveGShow ''ChannelMemberIdField1
deriveGEq ''ChannelMemberIdField1
deriveGCompare ''ChannelMemberIdField1

mkStandaloneDerivings [''Eq, ''Show] ''Db

-- Message

mkStandaloneDerivings [''Eq, ''Ord, ''Show] ''Message
mkTrivialInstances [''ToJSON, ''FromJSON] ''Message

mkStandaloneDerivings [''Eq, ''Ord, ''Show, ''ToJSON, ''FromJSON, ''ToJSONKey, ''FromJSONKey] 'MessageId

-- Channel

mkStandaloneDerivings [''Eq, ''Ord, ''Show] ''Channel
mkTrivialInstances [''ToJSON, ''FromJSON] ''Channel

mkStandaloneDerivings [''Eq, ''Ord, ''Show, ''ToJSON, ''FromJSON, ''ToJSONKey, ''FromJSONKey] 'ChannelId

-- ChannelMember

mkStandaloneDerivings [''Eq, ''Ord, ''Show] ''ChannelMember
mkTrivialInstances [''ToJSON, ''FromJSON] ''ChannelMember

mkStandaloneDerivings [''Eq, ''Ord, ''Show] 'ChannelMemberId
mkTrivialInstances [''ToJSON, ''FromJSON] 'ChannelMemberId
