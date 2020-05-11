module Test.Backend where

import Backend.Request
import Backend.Schema
import Backend.View
import Common.Api
import Common.Schema
import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.MVar.Restricted
import Control.Monad.GetEntropy
import Data.Dependent.Map (DSum (..))
import Data.IORef
import Data.Vessel
import Database.Beam.Backend.SQL
import Obelisk.Auth
import Obelisk.Auth.Bearer.Backend.Sign
import Obelisk.Auth.Bearer.Common.Sign
import Obelisk.View.Auth
import Obelisk.View.Api
import Obelisk.View.MultiMode
import Obelisk.View.Test.TempServer
import Obelisk.View.Coverage
import Obelisk.View.Coverable
import Obelisk.View.Sentinel
import qualified Data.Dependent.Map as DMap
import qualified Data.Map as Map
import Data.Time.Clock
import Test.Tasty
import Test.Tasty.HUnit
import Data.These
import Obelisk.Beam.View.Table
import Obelisk.View.Iv.Class
import Obelisk.View.Collidable
import Obelisk.View.IdentityInterface
import Obelisk.View.DMap
import Obelisk.Beam.Schema

import qualified Web.ClientSession as CS

tests :: [TestTree]
tests =
  [ testCase "basic" testDbView
  , testCase "latency" testLatency
  , testCase "switch" testSwitchAccount
  , testCase "sendSwitch" testSendSwitch
  , testCase "channelLoad" testChannelLoad
  {-
  , testCase "xorThing" $ do
      let x :: WithFullCoverage (Cov (Pull MyView))
          x = DefDMap Nothing (DMap.fromList [MyViewKey_Private :=> ComposeMaybe {getComposeMaybe = Just (DefMap Nothing (Map.fromList [(Right (AuthToken 1),Just (These () (This ())))]))}])
          y = DefDMap (Just ()) (DMap.fromList [MyViewKey_Private :=> ComposeMaybe {getComposeMaybe = Just (DefMap (Just (These () (These () (DefDMap (Just ()) (DMap.fromList []))))) (Map.fromList [(Right (AuthToken 1),Just (That (That (DefDMap (Just ()) (DMap.fromList [])))))]))}])
      let c = CoverageMap $ IntMap.fromList [(-9223372036854775808, x), (1, y)]
      True <- pure $ coverageMapFullCoveragesOnly c == CoverageMap (IntMap.singleton 1 ())
      pure ()
-}
  ]

setViewSelector
  :: (Cov (Pull i) -> Cov (Push i))
  -> ViewConnection i m
  -> Cov (Pull i)
  -> m ()
setViewSelector pullCovToPushCov conn vs =
  get conn (QueryDelta (Just $ pullCovToPushCov vs) Nothing, Just vs)

testDbView :: IO ()
testDbView = runCheckedServer $ \csk server -> do
  putStrLn "testDbView"
  token <- signWithKey csk $ AuthToken $ UserId 1
  _ <- runRequest server $ MyRequest_Private token $ PrivateMyRequest_Send (ChannelId 1) "Test Message0"

  threadDelay 2000000
  (setView, getView) <- manageView myViewPullCovToPushCov myViewPatchPullWithPush server
  let assertAccumEq expected = do
        --TODO: Write an explicit way of waiting until things settle (maybe by
        --transaction ID? or just generally wait for the whole IV to flush); then get
        --rid of all the threadDelays in this module that you can
        threadDelay 2000000 -- Wait for things to settle
        a <- getView
        when (a /= expected) $ do
          fail $ "Failed:\n  expected: " <> show expected <> "\n  actual:   " <> show a
  assertAccumEq mempty

  setView $ Just $ DMap.singleton MyViewKey_Private $ DPullQuery $ CovPull' $ Map.singleton token $ These (PossiblyFullCoverage_NonFull ()) $ PossiblyFullCoverage_NonFull $ DMap.singleton (MyViewKeyPrivate_ChannelMessages (ChannelId 1)) $ DPullQuery $ CovPull' $ Const ()

  _ <- runRequest server $ MyRequest_Private token $ PrivateMyRequest_Send (ChannelId 1) "Test Message1"
  _ <- runRequest server $ MyRequest_Private token $ PrivateMyRequest_Send (ChannelId 1) "Test Message2"
  pure ()

testLatency :: IO ()
testLatency = runCheckedServer $ \csk server -> do
  putStrLn "testLatency"
  t1Var <- newEmptyMVar
  token <- signWithKey csk $ AuthToken $ UserId 1
  let channel = ChannelId 1
  conn <- connectToView server (const $ pure ()) $ \v ->
    case DMap.lookup (MyViewKeyPrivate_ChannelMessages channel) =<< Map.lookup token . unPush'' . unDPush =<< DMap.lookup MyViewKey_Private v of
      Just _ -> putMVar t1Var =<< getCurrentTime
      _ -> pure ()
  _ <- setViewSelector myViewPullCovToPushCov conn $ DMap.singleton MyViewKey_Private $ DPullQuery $ CovPull' $ Map.singleton token $ These (PossiblyFullCoverage_NonFull ()) $ PossiblyFullCoverage_NonFull $ DMap.singleton (MyViewKeyPrivate_ChannelMessages channel) $ DPullQuery $ CovPull' $ Const ()
  threadDelay 2000000
  t0 <- getCurrentTime
  _ <- runRequest server $ MyRequest_Private token $ PrivateMyRequest_Send channel "Test Message"
  t1 <- takeMVar t1Var
  print $ t1 `diffUTCTime` t0
  threadDelay 2000000

manageView
  :: forall i r
  .  ( Coverable (Pull i)
     , Collidable (Pull i)
     , Show (Collision (Pull i))
     , Coverage (Cov (Pull i))
     )
  => (Cov (Pull i) -> Cov (Push i))
  -> (Push i -> Pull i -> Pull i)
  -> Server i r IO
  -> IO ( Maybe (Cov (Pull i)) -> IO ()
        , IO (Maybe (Pull i))
        )
manageView pullCovToPushCov patchPullWithPush server = do
  vRef :: IORef (Maybe (Pull i)) <- newIORef Nothing
  let
    addView :: Pull i -> IO ()
    addView incoming = atomicModifyIORef' vRef $ \(old :: Maybe (Pull i)) ->
      (Just $ maybe incoming (mergeOverwriting incoming) old :: Maybe (Pull i), ())

    patchView :: Push i -> IO ()
    patchView incoming = atomicModifyIORef' vRef $ \(old :: Maybe (Pull i)) ->
      (fmap (patchPullWithPush incoming) old, ())

  conn <- connectToView server addView patchView
  let
    setView vs = do
      -- TODO: This restriction logic actually needs to be applied more often because excess
      -- data could be received *after* we've cropped it, and that data should also be cropped out
      atomicModifyIORef' vRef $ \old -> (restrictMaybeCoverage vs old, ())
      mapM_ (setViewSelector pullCovToPushCov conn) vs

    getView =
      readIORef vRef

  pure (setView, getView)

expectIO :: (Eq a, Show a) => IO a -> a -> IO ()
expectIO getA expected = do
  v <- getA
  when (v /= expected) $ do
    fail $ "Failed:\n  expected: " <> show expected <> "\n  actual:   " <> show v

testSwitchAccount :: IO ()
testSwitchAccount = runCheckedServer $ \csk server -> do
  putStrLn "testSwitchAccount"
  token1 <- signWithKey csk $ AuthToken $ UserId 1
  token2 <- signWithKey csk $ AuthToken $ UserId 2
  let cid = ChannelId 1
  _ <- runRequest server $ MyRequest_Private token1 $ PrivateMyRequest_Send cid "Test Message"
  (setView, getView) <- manageView myViewPullCovToPushCov myViewPatchPullWithPush server
  setView $ Just $ DMap.singleton MyViewKey_Private $ DPullQuery $ CovPull' $ Map.singleton token1 $ These (PossiblyFullCoverage_NonFull ()) $ PossiblyFullCoverage_NonFull $ DMap.singleton (MyViewKeyPrivate_ChannelMessages cid) $ DPullQuery $ CovPull' $ Const ()
  threadDelay 2000000
  setView $ Just $ DMap.singleton MyViewKey_Private $ DPullQuery $ CovPull' $ Map.singleton token2 $ These (PossiblyFullCoverage_NonFull ()) $ PossiblyFullCoverage_NonFull $ DMap.singleton (MyViewKeyPrivate_ChannelMessages cid) $ DPullQuery $ CovPull' $ Const ()
  threadDelay 2000000
  setView $ Just $ DMap.singleton MyViewKey_Private $ DPullQuery $ CovPull' $ Map.singleton token1 $ These (PossiblyFullCoverage_NonFull ()) $ PossiblyFullCoverage_NonFull $ DMap.singleton (MyViewKeyPrivate_ChannelMessages cid) $ DPullQuery $ CovPull' $ Const ()
  threadDelay 2000000
  setView $ Just $ DMap.singleton MyViewKey_Private $ DPullQuery $ CovPull' $ Map.singleton token2 $ These (PossiblyFullCoverage_NonFull ()) $ PossiblyFullCoverage_NonFull $ DMap.singleton (MyViewKeyPrivate_ChannelMessages cid) $ DPullQuery $ CovPull' $ Const ()
  threadDelay 2000000
  expectIO getView $ Just $ DMap.singleton MyViewKey_Private $ DPull $ Pull'' $ Map.singleton token2 $ Either' $ Right $ These () $ DMap.singleton (MyViewKeyPrivate_ChannelMessages cid) $ DPull $ Pull'' $ TableView $ Map.singleton (MessageId $ SqlSerial 1) $ Message
    { _message_id = SqlSerial 1
    , _message_body = "Test Message"
    , _message_sender = UserId $ SqlSerial 1
    , _message_channel = ChannelId $ SqlSerial 1
    }

testSendSwitch :: IO ()
testSendSwitch = runCheckedServer $ \csk server -> do
  putStrLn "testSendSwitch: a"
  token1 <- signWithKey csk $ AuthToken $ UserId 1
  token2 <- signWithKey csk $ AuthToken $ UserId 2
  let cid = ChannelId 1
  (setView, getView) <- manageView myViewPullCovToPushCov myViewPatchPullWithPush server
  putStrLn "testSendSwitch: b"
  setView $ Just $ DMap.singleton MyViewKey_Private $ DPullQuery $ CovPull' $ Map.singleton token1 $ These (PossiblyFullCoverage_NonFull ()) $ PossiblyFullCoverage_NonFull $ DMap.singleton (MyViewKeyPrivate_ChannelMessages cid) $ DPullQuery $ CovPull' $ Const ()
  putStrLn "testSendSwitch: c"
  threadDelay 2000000
  _ <- runRequest server $ MyRequest_Private token1 $ PrivateMyRequest_Send (ChannelId 1) "Test Message"
  putStrLn "testSendSwitch: d"
  threadDelay 20000000
  putStrLn "testSendSwitch: checking output"
  expectIO getView $ Just $ DMap.singleton MyViewKey_Private $ DPull $ Pull'' $ Map.singleton token1 $ Either' $ Right $ These () $ DMap.singleton (MyViewKeyPrivate_ChannelMessages cid) $ DPull $ Pull'' $ TableView $ Map.singleton (MessageId $ SqlSerial 1) $ Message
    { _message_id = SqlSerial 1
    , _message_body = "Test Message"
    , _message_sender = UserId $ SqlSerial 1
    , _message_channel = ChannelId $ SqlSerial 1
    }
  setView mempty
  putStrLn "testSendSwitch: e"
  threadDelay 2000000
  setView $ Just $ DMap.singleton MyViewKey_Private $ DPullQuery $ CovPull' $ Map.singleton token2 $ These (PossiblyFullCoverage_NonFull ()) $ PossiblyFullCoverage_NonFull $ DMap.singleton (MyViewKeyPrivate_ChannelMessages cid) $ DPullQuery $ CovPull' $ Const ()
  putStrLn "testSendSwitch: f"
  threadDelay 20000000
  expectIO getView $ Just $ DMap.singleton MyViewKey_Private $ DPull $ Pull'' $ Map.singleton token2 $ Either' $ Right $ These () $ DMap.singleton (MyViewKeyPrivate_ChannelMessages cid) $ DPull $ Pull'' $ TableView $ Map.singleton (MessageId $ SqlSerial 1) $ Message
    { _message_id = SqlSerial 1
    , _message_body = "Test Message"
    , _message_sender = UserId $ SqlSerial 1
    , _message_channel = ChannelId $ SqlSerial 1
    }
  putStrLn "testSendSwitch: g"

testChannelLoad :: IO ()
testChannelLoad = runCheckedServer $ \csk server -> do
  let userName = "Test User"
      userEmail = "test@example.com"
  runRequest server $ MyRequest_Public $ PublicMyRequest_Signup userName userEmail
  let uid = UserId $ SqlSerial 1
      channelName = "Test Channel"
      messageBody = "Test Message"
  token <- signWithKey csk $ AuthToken uid
  cid <- runRequest server $ MyRequest_Private token $ PrivateMyRequest_AddChannel channelName
  mid <- runRequest server $ MyRequest_Private token $ PrivateMyRequest_Send cid messageBody
  (setView, getView) <- manageView myViewPullCovToPushCov myViewPatchPullWithPush server
  let vs1 = DMap.singleton MyViewKey_Private $ DPullQuery $ CovPull' $ Map.singleton token $ These (PossiblyFullCoverage_NonFull ()) $ PossiblyFullCoverage_NonFull $ DMap.fromList
        [ MyViewKeyPrivate_ChannelMessages cid :=> DPullQuery (CovPull' $ Const ())
        , MyViewKeyPrivate_MyChannels :=> DPullQuery (CovPull' $ Const ())
        ]
  putStrLn "testChannelLoad: fetching messages"
  setView $ Just vs1
  threadDelay 10000000
  expectIO getView $ Just $ DMap.singleton MyViewKey_Private $ DPull $ Pull'' $ Map.singleton token $ Either' $ Right $ These () $ DMap.fromList
    [ (MyViewKeyPrivate_ChannelMessages cid :=>) $ DPull $ Pull'' $ TableView $ Map.singleton mid $ Message
      { _message_id = _messageId_id mid
      , _message_body = messageBody
      , _message_sender = uid
      , _message_channel = cid
      }
    , (MyViewKeyPrivate_MyChannels :=>) $ DPull $ Pull'' $ TableView $ Map.singleton (CrossJoinId $ cid :*: ChannelMemberId cid uid)$ (Channel
      { _channel_id = _channelId_id cid
      , _channel_name = channelName
      } :*: ChannelMember
      { _channelMember_channel = cid
      , _channelMember_user = uid
      , _channelMember_isOperator = True
      })
    ]
  putStrLn "testChannelLoad: fetching users"
  let vs2 = DMap.singleton MyViewKey_Private $ DPullQuery $ CovPull' $ Map.singleton token $ These (PossiblyFullCoverage_NonFull ()) $ PossiblyFullCoverage_NonFull $ DMap.fromList
        [ MyViewKeyPrivate_ChannelMessages cid :=> DPullQuery (CovPull' $ Const ())
        , MyViewKeyPrivate_MyChannels :=> DPullQuery (CovPull' $ Const ())
        , MyViewKeyPrivate_User uid :=> DPullQuery (CovPull' $ Const ())
        ]
  setView $ Just vs2
  threadDelay 20000000
  expectIO getView $ Just $ DMap.singleton MyViewKey_Private $ DPull $ Pull'' $ Map.singleton token $ Either' $ Right $ These () $ DMap.fromList
    [ (MyViewKeyPrivate_ChannelMessages cid :=>) $ DPull $ Pull'' $ TableView $ Map.singleton mid $ Message
      { _message_id = _messageId_id mid
      , _message_body = messageBody
      , _message_sender = uid
      , _message_channel = cid
      }
    , (MyViewKeyPrivate_MyChannels :=>) $ DPull $ Pull'' $ TableView $ Map.singleton (CrossJoinId $ cid :*: ChannelMemberId cid uid)$ (Channel
      { _channel_id = _channelId_id cid
      , _channel_name = channelName
      } :*: ChannelMember
      { _channelMember_channel = cid
      , _channelMember_user = uid
      , _channelMember_isOperator = True
      })
    , (MyViewKeyPrivate_User uid :=>) $ DPull $ Pull'' $ TableView $ Map.singleton uid $ User
      { _user_id = _channelId_id cid
      , _user_name = userName
      , _user_email = userEmail
      , _user_passwordHash = Nothing
      }
    ]

runCheckedServer
  :: (CS.Key -> Server MyView (MyRequest (Signed AuthToken)) IO -> IO a)
  -> IO a
runCheckedServer go = do
  numWarningsRef <- newIORef (0 :: Integer)
  Right !csk <- CS.initKey <$> getEntropy 96
  let warn s = do
        atomicModifyIORef' numWarningsRef $ \old -> (succ old, ())
        putStr s
  result <- withTempServer checkedDb (runCheckedIv warn $ unObeliskIv @'Checked (myView csk)) (handleRequest csk) (go csk)
  numWarnings <- readIORef numWarningsRef
  when (numWarnings /= 0) $ do
    fail $ "Received " <> show numWarnings <> " warnings from CheckedIv during test"
  pure result
