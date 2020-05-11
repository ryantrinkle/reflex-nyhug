module Backend.Jobs where

import Prelude hiding ((.), id)

import Backend.Schema
import Common.Route
import Common.Schema
import Control.Category
import Data.ByteString (ByteString)
import Data.Functor.Identity
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Database.Beam.Postgres
import Obelisk.Auth.SecureChannel.Common.Schema
import Obelisk.Beam.Schema
import Obelisk.Route
import Obelisk.View.Beam
import Obelisk.View.Iv.Base
import Data.Pool
import Obelisk.Beam.Patch.Types
import Obelisk.Email
import Network.Mail.Mime (Mail(..), plainPart)
import Network.Mail.SMTP
import Obelisk.View.Iv.Class
import Obelisk.View.Unitary

import Obelisk.Auth
import qualified Obelisk.Auth.SecureChannel.Backend.Jobs

data Job
   = Job_EmailAuth (Obelisk.Auth.SecureChannel.Backend.Jobs.Job (Column EmailAddress))
   deriving (Show)

subDb :: FieldChain Db (Obelisk.Auth.SecureChannel.Common.Schema.SecureChannelAuth (Column EmailAddress)) -> Iv IO (BeamDbInterface' TableQ Postgres Db) (BeamDbInterface' TableQ Postgres (Obelisk.Auth.SecureChannel.Common.Schema.SecureChannelAuth (Column EmailAddress)))
subDb = subDb'

todo :: Iv IO (BeamDbInterface Postgres Db) (LiftInterface (Unitary (Maybe Job)))
todo =
  mapUnitaryIv (fmap Job_EmailAuth) (fmap Job_EmailAuth) .
  Obelisk.Auth.SecureChannel.Backend.Jobs.todo @(Column EmailAddress) @EmailAddress .
  subDb (fieldToChain DbField1_EmailAuth)

runJob
  :: Pool Connection
  -> Encoder Identity Identity (R (FullRoute a FrontendRoute)) PageName
  -> Text -- ^ Route
  -> EmailConfig
  -> Job
  -> IO ()
runJob dbPool validEncoder route emailConfig = \case
  Job_EmailAuth j -> Obelisk.Auth.SecureChannel.Backend.Jobs.runJob (subDbSettings _db_emailAuth db) dbPool (\address nonce -> sendEmail_ emailConfig $ passwordResetEmail address nonce) j
  where
    passwordResetEmail :: Column Text Identity -> ByteString -> Mail
    passwordResetEmail address nonce = simpleMail
      (Address Nothing "noreply@obsidian.systems")
      [Address Nothing $ unColumn address]
      []
      []
      "Password Reset"
      [ plainPart $ TL.fromStrict $ (route <>) $ renderFrontendRoute validEncoder $ FrontendRoute_Auth :/ SecureChannelResetRoute_Email :/ nonce
      ]
