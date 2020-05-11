module Backend where

import Prelude hiding ((.), id)
import Control.Category ((.), id)

import Backend.Request
import Backend.Schema
import Backend.View
import Common.Route
import Data.Bifunctor (first)
import Obelisk.Backend
import Obelisk.Route
import Obelisk.Configs
import Obelisk.View.Backend
import Obelisk.View.MultiMode
import Obelisk.View.Sentinel
import System.Environment
import Control.Exception
import Backend.Jobs
import Obelisk.View.Inspectable
import Obelisk.Beam.Orphans ()

-- To prevent tests from bitrotting:
import Obelisk.View.Test ()
import qualified Test
import qualified Test.Backend ()
import qualified Benchmark.Backend ()

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Snap.Core (pass, writeBS)

import Obelisk.Email

import Data.Dependent.Sum.Orphans ()

import qualified Web.ClientSession as CS

--TODO: Ensure we only send full transactions worth of data to the frontend (?)

--TODO: What do we do about large transactions?  We can't afford to buffer the whole thing.

--TODO: Test in the presence of misbehaving clients (e.g. dropped connections, etc.)

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      (!emailConfig) :: EmailConfig <- processConfig (Aeson.eitherDecode . LBS.fromStrict) getConfig "backend/email"
      !route <- processConfig (Right . T.strip) getTextConfig "common/route"
      !csk <- processConfig CS.initKey getConfig "backend/clientSessionKey"
      -- TODO Obelisk should provide the checked encoder
      let Right !validEncoder = checkEncoder fullRouteEncoder
      --TODO: Tests should be run by `ob run`, not here
      _ <- liftIO $ try @SomeException $ when False $ withArgs ["-p", ""] Test.main
      (view, getStatus) <- liftIO
        $ fmap (first (ObeliskIv @'Plain))
        $ runInspectableIv
        -- Uncomment this line to enable the Sentinel IV tracking.
        --  $ sentinelView traceM
        $ unObeliskIv @'Inspectable
        $ myView csk
      --TODO: Actually authenticate requests
      withIvServer "db" checkedDb todo (\dbPool -> runJob dbPool validEncoder route emailConfig) view (handleRequest csk) $ \apiHandler -> serve $ \case
        BackendRoute_Api :/ () -> lift apiHandler
        BackendRoute_Status :/ () -> do
          writeBS =<< liftIO . toHtml =<< liftIO getStatus
        BackendRoute_Missing :/ () -> pass
  , _backend_routeEncoder = fullRouteEncoder
  }
  where
    processConfig :: (Functor m) => (a -> Either String b) -> (T.Text -> m (Maybe a)) -> T.Text -> m b
    processConfig parse getCfg file = let fileStr = T.unpack file in
      maybe
        (error $ "Config not found: " <> fileStr)
        (either (\err -> error $ "Failed to parse " <> fileStr <> ": " <> err) id . parse)
      <$> getCfg file
