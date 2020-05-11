module Backend.Request where

import Prelude hiding (id, (.))

import Backend.Schema
import Common.Api
import Common.Schema
import Control.Monad.GetEntropy
import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Postgres.Full (returning)
import Data.Time
import Obelisk.Api
import Obelisk.Auth
import Obelisk.Auth.Bearer.Common.Sign
import Obelisk.Auth.Bearer.Backend.Sign
import Obelisk.Auth.EmailAndPassword.Backend
import Obelisk.Auth.SecureChannel.Common.Schema
import Obelisk.Beam
import Obelisk.Beam.Schema
import Database.Beam.Postgres.Syntax
import qualified Web.ClientSession as CS

-- | Postgres @current_timestamp()@ function. Returns the server's timestamp
-- TODO: Upstream this. Beam has 'now_' but that returns a 'LocalTime'
current_timestamp_ :: QExpr Postgres s UTCTime
current_timestamp_ = QExpr (\_ -> PgExpressionSyntax (emit "current_timestamp at time zone 'UTC'"))

--TODO: Factor out authentication into standard Obelisk module
handleRequest :: CS.Key -> MyRequest (Signed AuthToken) a -> WriteDb a
handleRequest csk = \case
  MyRequest_Public req -> case req of
    PublicMyRequest_Signup handle email -> do
      runInsert' $ insert (_db_users db) $ insertExpressions $ (:[]) $ User
        { _user_id = default_
        , _user_name = val_ handle
        , _user_email = val_ email
        , _user_passwordHash = val_ Nothing
        }
      -- 8 bytes of entropy should really be enough - see
      -- https://security.stackexchange.com/questions/1952/how-long-should-a-random-nonce-be
      -- - so we go with 18 to provide some headroom.  Multiples of 3 look
      -- nicest, because later we will base64-encode this
      nonce <- getEntropy 18
      runInsert' $ insert (_secureChannelAuth_resets $ _db_emailAuth db) $ insertExpressions $ (:[]) $ SecureChannelReset
        { _secureChannelReset_address = val_ $ Column email
        , _secureChannelReset_nonce = val_ nonce
        , _secureChannelReset_expiration = current_timestamp_
        , _secureChannelReset_startedSending = val_ False
        , _secureChannelReset_sent = val_ False
        }
      pure ()
    PublicMyRequest_Auth authReq -> authHandler (_db_users db) (_db_emailAuth db) csk authReq
  MyRequest_Private token req -> do
    Right (AuthToken authenticatedUser) <- pure $ authenticateToken csk token
    case req of
      PrivateMyRequest_AddChannel name -> do
        [channelId] <- runInsertReturningList' $ flip returning _channel_id $ insert (_db_channels db) $ insertExpressions $ (:[]) $ Channel
          { _channel_id = default_
          , _channel_name = val_ name
          }
        runInsert' $ insert (_db_channelMembers db) $ insertExpressions $ (:[]) $ ChannelMember
          { _channelMember_channel = val_ $ ChannelId channelId
          , _channelMember_user = val_ authenticatedUser
          , _channelMember_isOperator = val_ True -- channel creator is always an operator.
          }
        pure $ ChannelId channelId
      PrivateMyRequest_Send channel body -> do
        [messageId] <- runInsertReturningList' $ flip returning _message_id $ insert (_db_messages db) $ insertExpressions $ (:[]) $ Message
          { _message_id = default_
          , _message_body = val_ body
          , _message_sender = val_ authenticatedUser
          , _message_channel = val_ channel
          }
        pure $ MessageId messageId
