{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Web.Twitter.Conduit hiding (map)
import Web.Twitter.Types.Lens

import Control.Applicative
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Data.Aeson
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as BS
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Data.Default
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.HTTP.Conduit
import System.IO (hFlush, stdout)
import Web.Authenticate.OAuth (OAuth(..), Credential(..))
import qualified Web.Authenticate.OAuth as OA

import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe

site :: Snap ()
site = (serveDirectory "../static") <|>
       route [ ("oauth", getOauthUrl)
             , ("blank", writeBS "")
             , ("twitter/secret", getTwitterSecret)
             , ("twitter/timeline", getTwitterTimeline)
             ]

tokens :: OAuth
tokens = twitterOAuth
    { oauthConsumerKey = "7R6EhcPa9ypCPD3ifqxD9sZYS"
    , oauthConsumerSecret = "McRPjCROcKwS9A4HR3N7ivjZqlZYbPuTQmHFy0ick1oLIvtR9W"
    , oauthCallback = Just "http://localhost:8000/blank"
    }

authorize :: (MonadBaseControl IO m, MonadResource m)
          => OAuth -- ^ OAuth Consumer key and secret
          -> (String -> m String) -- ^ PIN prompt
          -> Manager
          -> m Credential
authorize oauth getPIN mgr = do
    cred <- OA.getTemporaryCredential oauth mgr
    let url = OA.authorizeUrl oauth cred
    pin <- getPIN url
    OA.getAccessToken oauth (OA.insert "oauth_verifier" (B8.pack pin) cred) mgr

getTWInfo :: IO TWInfo
getTWInfo = do
    cred <- withManager $ \mgr -> authorize tokens getPIN mgr
    return $ setCredential tokens cred def
  where
    getPIN url = liftIO $ do
        putStrLn $ "browse URL: " ++ url
        putStr "> what was the PIN twitter provided you with? "
        hFlush stdout
        getLine

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
  params <- getQueryParams
  r <- liftIO $ withManager $ \mgr -> OA.getAccessToken tokens (toCred params) mgr
  writeText $ T.pack $ show $ unCredential r

toCred :: Map BS.ByteString [BS.ByteString] -> Credential
toCred ps = Credential $ concatMap (\(a,b) -> map (\c -> (a,c)) b) $ Map.toList ps

getTwitterTimeline :: Snap ()
getTwitterTimeline = do
  params <- getQueryParams
  let cred = setCredential tokens (toCred params) def
  htl <- liftIO $ timeline cred
  writeLBS $ encode htl

timeline :: TWInfo -> IO [Status]
timeline twInfo = withManager $ \mgr -> call twInfo mgr homeTimeline
