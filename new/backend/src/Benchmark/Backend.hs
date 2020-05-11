{-# OPTIONS_GHC -fprof-cafs #-}
module Benchmark.Backend where

import Obelisk.Beam.DZippable
import Obelisk.Beam.Constraints
import Obelisk.Beam.TablesOnly
import Obelisk.Beam.TablesV
import Backend.Request
import Backend.Schema
import Backend.View
import Common.Api
import Common.Schema
import Criterion.Main
import Data.Bifunctor (first)
import Data.IORef
import Data.Maybe
import Control.Monad
import Control.Monad.GetEntropy
import Control.Monad.MVar.Restricted
import Obelisk.Auth
import Obelisk.Auth.Bearer.Backend.Sign
import Obelisk.Auth.Bearer.Common.Sign
import Obelisk.Beam.Patch.Row
import Obelisk.View.Auth
import Obelisk.View.Api
import Obelisk.View.MultiMode
import Obelisk.View.DMap
import Obelisk.View.Test.TempServer
import Database.Beam.Backend.SQL
import Obelisk.Beam.Patch.Db
import Data.Time.Clock
import qualified Data.Text as T
import Data.Functor.Const
import Data.Functor.Identity
import Data.Monoid
import Test.Backend
import Control.Concurrent (threadDelay)
import Data.These
import qualified Data.Dependent.Map as DMap
import qualified Data.Map as Map
import Data.Dependent.Sum (DSum (..))
import Control.Concurrent.Async
import Control.Lens ((^?), _Right, Lens', (.~))
import Data.These.Lens
import Control.Monad.State (State, runState, modify, gets)
import Database.Beam
import Data.Patch
import Data.Patch.MapWithPatchingMove
import Data.Functor.Misc
import Data.Functor.Compose
import Obelisk.View.Sentinel
import qualified Data.ByteString as BS
import Obelisk.View.Inspectable
import qualified Web.ClientSession as CS
import Obelisk.Beam.View.Db
import Obelisk.Beam.Patch.Table
import Obelisk.Beam.View.Table
import Obelisk.View.IdentityInterface
import Obelisk.View.Unitary
import System.IO

benchmarks :: [Benchmark]
benchmarks =
  [ bench "count10x10" $ nfIO $ countNxN 10 10
  , bench "count10x100" $ nfIO $ countNxN 10 100
  , bench "count100x10" $ nfIO $ countNxN 100 10
  , bench "count100x100" $ nfIO $ countNxN 100 100
  , bench "count100x100000" $ nfIO $ countNxN 100 100000
  , bench "send100x1x1" $ nfIO $ sendNxN 100 1 1
  , bench "send0x0x0" $ nfIO $ sendNxN 0 0 0
  , bench "send1x1x1" $ nfIO $ sendNxN 1 1 1
  , bench "send2x2x2" $ nfIO $ sendNxN 2 2 2
  , bench "send100x100x100" $ nfIO $ sendNxN 100 100 100
  , bench "send100x100x10000" $ nfIO $ sendNxN 100 100 10000
  ]

countNxN :: Int -> Int -> IO ()
countNxN numClients numSends = do
  Right csk <- CS.initKey <$> getEntropy 96
  withTempServer checkedDb (unObeliskIv @'Plain (myView csk)) (handleRequest csk) $ \server -> do
    clientsNotDone <- newIORef numClients
    allClientsDone <- newEmptyMVar
    replicateM_ numClients $ do
      sendsRemaining <- newIORef numSends
      let numFromPull v = fromMaybe 0 $ fmap (unUnitary . unPull'' . unDPull) . DMap.lookup MyViewKeyPublic_NumMessages . unPull'' . unDPull =<< DMap.lookup MyViewKey_Public v
          numFromPush v = fromMaybe 0 $ fmap (getSum . unUnitary . unPush'' . unDPush) . DMap.lookup MyViewKeyPublic_NumMessages . unPush'' . unDPush =<< DMap.lookup MyViewKey_Public v
      let g = \sentThisTime -> do
            -- The hack of counting pulls and pushes the same way only works because we only get `Complete` once in this test
  --          putStrLn $ "Client " <> show clientNum <> " received " <> show sentThisTime
            isDone <- atomicModifyIORef' sendsRemaining $ \old ->
              let new = old - sentThisTime
              in (new, new == 0)
            when isDone $ do
  --            putStrLn $ "Client " <> show clientNum <> " is done"
              allAreDone <- atomicModifyIORef' clientsNotDone $ \old ->
                let new = pred old
                in (new, new == 0)
              when allAreDone $ do
  --              putStrLn "All clients are done"
                putMVar allClientsDone ()
      conn <- connectToView server (g . numFromPull) (g . numFromPush)
      let vs = DMap.singleton MyViewKey_Public $ DPullQuery $ CovPull' $ DMap.singleton MyViewKeyPublic_NumMessages $ DPullQuery $ CovPull' ()
      get conn (QueryDelta (Just $ myViewPullCovToPushCov vs) Nothing, Just vs)
    --TODO: Do in parallel
    replicateM_ numSends $ do
      token <- signWithKey csk $ AuthToken $ UserId 1
      runRequest server $ MyRequest_Private token $ PrivateMyRequest_Send (ChannelId $ SqlSerial 1) "Message"
    takeMVar allClientsDone

-- | Accumulate the current state of the db as well as the patch that got it there
newtype WriteMemoryDb db a = WriteMemoryDb { unWriteMemoryDb :: State (DbView db, DbPatchV db TablePatch Identity) a }
  deriving (Functor, Applicative, Monad)

runWriteMemoryDb
  :: ( HasT IsTable db
     , DZippable db
     )
  => WriteMemoryDb db a
  -> DbView db
  -> (DbPatchV db TablePatch Identity, a)
runWriteMemoryDb (WriteMemoryDb a) d = (change, result)
  where (result, (_, change)) = runState a (d, emptyDbPatchV)

patchMemoryDb
  :: ( HasT' Semigroup db (TableOnly (ComposeMaybe (Compose Identity TablePatch)))
     , HasT IsTable db
     , HasT (TableHas_ BeamableOrdPrimaryKey) db
     , DZippable db
     )
  => DbPatchV db TablePatch Identity
  -> WriteMemoryDb db ()
patchMemoryDb p = WriteMemoryDb $ modify $ \(oldD, oldP) -> (applyAlways p oldD, p <> oldP)

getMemoryDb :: WriteMemoryDb db (DbView db)
getMemoryDb = WriteMemoryDb $ gets fst

insertMemoryDb
  :: ( HasT' Semigroup db (TableOnly (ComposeMaybe (Compose Identity TablePatch)))
     , HasT IsTable db
     , HasT (TableHas_ BeamableOrdPrimaryKey) db
     , DZippable db
     , Table tbl
     )
  => (forall f. Lens' (db f) (f (TableEntity tbl)))
  -> tbl Identity
  -> WriteMemoryDb db ()
insertMemoryDb tbl r = patchMemoryDb $ DbPatchV $ TablesV $ (tbl .~ tblPatch) empty
  where DbPatchV (TablesV empty) = emptyDbPatchV
        tblPatch = TableOnly $ ComposeMaybe $ Just $ Compose $ Identity $ TablePatch $ patchMapWithPatchingMoveInsertAll $ Map.singleton (primaryKey r) r

handleRequestMemory :: CS.Key -> MyRequest (Signed AuthToken) a -> DbView Db -> (DbPatchV Db TablePatch Identity, a)
handleRequestMemory csk = runWriteMemoryDb . \case
  MyRequest_Public req -> case req of
    PublicMyRequest_Signup handle email -> insertMemoryDb db_users $ User
      { _user_id = 1 --TODO
      , _user_name = handle
      , _user_email = email
      , _user_passwordHash = Nothing
      }
    PublicMyRequest_Auth {} -> undefined
  MyRequest_Private token req -> do
    case authenticateToken csk token of
      Left _ -> error "bad token"
      Right (AuthToken authenticatedUser) -> case req of
        PrivateMyRequest_AddChannel name -> do
          let myId = 1
          insertMemoryDb db_channels $ Channel
            { _channel_id = myId --TODO
            , _channel_name = name
            }
          pure $ ChannelId myId
        PrivateMyRequest_Send channel body -> do
          DbView (TablesV d) <- getMemoryDb
          let myId = maybe 1 (succ . _messageId_id . fst) $ Map.lookupMax $ unTableView $ unTableOnly $ _db_messages d
          insertMemoryDb db_messages $ Message
            { _message_id = myId --TODO
            , _message_body = body
            , _message_sender = authenticatedUser
            , _message_channel = channel
            }
          pure $ MessageId myId

sendNxN :: Int -> Int -> Int -> IO ()
sendNxN numClients numSendThreads numSendsPerThread = do
  Right csk <- CS.initKey <$> getEntropy 96
  latenciesRef :: IORef [NominalDiffTime] <- newIORef []
  (view, getStatus) <- fmap (first (ObeliskIv @'Plain)) $ runInspectableIv $ unObeliskIv @'Inspectable $ myView csk
  withTempServer checkedDb (unObeliskIv @'Plain view) (handleRequest csk) $ \server -> do
    let userName = "Test User"
        userEmail = "test@example.com"
    runRequest server $ MyRequest_Public $ PublicMyRequest_Signup userName userEmail
    let uid = UserId $ SqlSerial 1 :: PrimaryKey User Identity
        channelName = "Test Channel"
        -- messageBody = "Test Message"
    token <- signWithKey csk $ AuthToken uid
    cid <- runRequest server $ MyRequest_Private token $ PrivateMyRequest_AddChannel channelName
    let client shouldLog = do
          vRef :: IORef Int <- newIORef 0
          let extractPullCount = fromMaybe 0 . ((fmap (Map.size . unTableView . unPull'' . unDPull) . DMap.lookup (MyViewKeyPrivate_ChannelMessages cid) <=< (^? _Right . there) . unEither' <=< Map.lookup token . unPull'' . unDPull <=< DMap.lookup MyViewKey_Private))
          -- NOTE: extractPushCount assumes that all changes are inserts
          let
            extractMessageMap
              :: DMap.DMap (MyViewKey (Signed AuthToken)) (DPush IdentityInterface)
              -> Maybe (Map.Map
                         (PrimaryKey Message Identity)
                         (NodeInfo (PrimaryKey Message Identity) (RowPatch Message)))
            extractMessageMap = fmap (unPatchMapWithPatchingMove . unTablePatch . unPush'' . unDPush) .
              DMap.lookup (MyViewKeyPrivate_ChannelMessages cid) <=<
              Map.lookup token . unPush'' . unDPush <=<
              DMap.lookup MyViewKey_Private
          let extractPushCount = fromMaybe 0 . fmap Map.size . extractMessageMap
          let extractPushTimestamps = fromMaybe [] . fmap (map go . Map.elems) . extractMessageMap
              go x = case _nodeInfo_from x of
                From_Delete -> error "unexpected deletion patch"
                From_Move _ _ -> error "unexpected move patch"
                From_Insert p -> read . T.unpack . _message_body $ p :: UTCTime
          let addView incoming = do
                join $ atomicModifyIORef' vRef $ \old ->
                  let c = extractPullCount (incoming)
                  in ( old + c
                     , when shouldLog $ do
                        putStrLn $ "client: " <> show c
                     )
              patchView incoming = do
                receiveTime <- getCurrentTime
                join $ atomicModifyIORef' vRef $ \old ->
                  let c = extractPushCount incoming
                      messageLatencies = fmap (diffUTCTime receiveTime) $ extractPushTimestamps incoming
                  in
                    ( old + c
                    , when shouldLog $ do
                      forM_ messageLatencies $ \latency -> do
                        atomicModifyIORef' latenciesRef $ \ts -> (latency : ts, ())
                      putStrLn $ "client: " <> show c
                    )
          conn <- connectToView server addView patchView
          let setView vs = do
                mapM_ (setViewSelector myViewPullCovToPushCov conn) vs
          setView $ Just $ DMap.singleton MyViewKey_Private $ DPullQuery $ CovPull' $ Map.singleton token $ These (PossiblyFullCoverage_NonFull ()) $ PossiblyFullCoverage_NonFull $ DMap.fromList
            [ MyViewKeyPrivate_ChannelMessages cid :=> DPullQuery (CovPull' $ Const ())
            , MyViewKeyPrivate_MyChannels :=> DPullQuery (CovPull' $ Const ())
            ]
          pure $ readIORef vRef
    replicateM_ (numClients - 1) $ do
      void $ client False
    getMaster <- client True
    threadDelay 1000000
    replicateConcurrently_ numSendThreads $ do
      replicateM_ numSendsPerThread $ do
        sendTime <- getCurrentTime
        _ <- runRequest server $ MyRequest_Private token $ PrivateMyRequest_Send cid $ T.pack $ show sendTime
        threadDelay 10000
    let loop = do
          putStrLn "Writing sendNxN.html"
          BS.writeFile "sendNxN.html" =<< toHtml =<< getStatus
          putStrLn "Wrote sendNxN.html"
          threadDelay 1000000
          numCollectedByMaster <- getMaster
          print numCollectedByMaster
          if numCollectedByMaster /= numSendThreads * numSendsPerThread
            then loop
            else pure ()
    loop
  latencies <- reverse <$> readIORef latenciesRef
  writeFile "latencies.csv" $ foldMap ((<> "\n") . show) latencies
  putStrLn . ("Avg latency: " <>) . show @NominalDiffTime $ sum (realToFrac <$> latencies) / (realToFrac $ length latencies)
  hFlush stdout
