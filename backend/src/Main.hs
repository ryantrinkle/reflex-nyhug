{-# LANGUAGE OverloadedStrings, FlexibleContexts, ScopedTypeVariables #-}

module Main where

import Web.Twitter.Conduit hiding (map)

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Data.Aeson
import Data.Bifunctor
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Default
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding
import Network.HTTP.Conduit
import Web.Authenticate.OAuth (OAuth(..), Credential(..))
import qualified Web.Authenticate.OAuth as OA

import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe
import Network.WebSockets
import Network.WebSockets.Snap

site :: Snap ()
site = (serveDirectory "../static") <|>
       route [ ("oauth", getOauthUrl)
             , ("blank", writeBS "")
             , ("twitter/secret", getTwitterSecret)
             , ("twitter/timeline", getTwitterTimeline)
             , ("twitter/status", postTwitterStatus)
             , ("twitter/search", getSearchResults)
             , ("twitter/userStream", userStream)
             ]

tokens :: OAuth
tokens = twitterOAuth
    { oauthConsumerKey = "7R6EhcPa9ypCPD3ifqxD9sZYS"
    , oauthConsumerSecret = "McRPjCROcKwS9A4HR3N7ivjZqlZYbPuTQmHFy0ick1oLIvtR9W"
    , oauthCallback = Just "http://localhost:8000/blank"
    }

main :: IO ()
main = do
  quickHttpServe site

getOauthUrl :: Snap ()
getOauthUrl = do
  mcb <- getQueryParam "callback"
  let tokensWithCallback = case mcb of
        Nothing -> tokens
        Just cb -> tokens { oauthCallback = Just cb }
  cred <- liftIO $ withManager $ \mgr -> OA.getTemporaryCredential tokensWithCallback mgr
  writeText $ T.pack $ OA.authorizeUrl tokens cred

getTwitterSecret :: Snap ()
getTwitterSecret = do
  ps <- getQueryParams
  r <- liftIO $ withManager $ \mgr -> OA.getAccessToken tokens (toCred ps) mgr
  writeText $ T.pack $ show $ unCredential r

toCred :: Map BS.ByteString [BS.ByteString] -> Credential
toCred ps = Credential $ concatMap (\(a,b) -> map (\c -> (a,c)) b) $ Map.toList ps

getTwitterTimeline :: Snap ()
getTwitterTimeline = do
  ps <- getQueryParams
  htl <- liftIO $ makeCall (toCred ps) $ homeTimeline & count ?~ 25
  writeLBS $ encode htl

postTwitterStatus :: Snap ()
postTwitterStatus = do
  rb <- readRequestBody 10000
  let (cred, tweet) :: (Credential, Text) = first Credential $ readLBS rb
  s <- liftIO $ makeCall cred $ update tweet
  writeLBS $ encode s
  return ()

getSearchResults :: Snap ()
getSearchResults = do
  rb <- readRequestBody 10000
  let (cred, query) :: (Credential, Text) = first Credential $ readLBS rb
  s <- liftIO $ makeCall cred $ searchTweets query & count ?~ 25
  writeLBS $ encode s

userStream :: Snap ()
userStream = do
  setTimeout 100000
  ps <- getQueryParams
  s <- liftIO $ newEmptyMVar
  _ <- liftIO $ forkIO $ withManager $ \mgr -> do
    u <- stream (setCredential tokens (toCred ps) def) mgr userstream
    _ <- (C.$$++) u (CL.mapM_ (liftIO . putMVar s))
    return ()
  runWebSocketsSnap $ \pc -> do
    conn <- acceptRequest pc
    senderThread <- forkIO $ do
      let sendws = sendBinaryData conn . encode
      forever $ do
        change <- takeMVar s
        sendws change
    let handleConnectionException = handle $ \e -> case e of
          ConnectionClosed -> return ()
          _ -> print e
    handleConnectionException $ forever $ receiveDataMessage conn
    killThread senderThread
  return ()

makeCall :: forall m a apiName. (MonadIO m, MonadBaseControl IO m, MonadThrow m, FromJSON a) => Credential -> APIRequest apiName a -> m a
makeCall cred apiCall = withManager $ \mgr -> call (setCredential tokens cred def) mgr apiCall

readLBS :: Read a => LBS.ByteString -> a
readLBS = read . T.unpack . decodeUtf8 . LBS.toStrict
