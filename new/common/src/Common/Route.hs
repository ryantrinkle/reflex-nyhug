{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Common.Route where

import Prelude hiding (id, (.))
import Control.Category

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Functor.Identity

import Obelisk.Beam.Schema
import Obelisk.Route
import Obelisk.Route.TH
import Database.Beam
import Common.Schema

data SecureChannelResetRoute :: * -> * where
  SecureChannelResetRoute_Email :: SecureChannelResetRoute ByteString

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()
  -- You can define any routes that will be handled specially by the backend here.
  -- i.e. These do not serve the frontend, but do something different, such as serving static files.
  BackendRoute_Api :: BackendRoute ()
  BackendRoute_Status :: BackendRoute ()

-- This type is used to define frontend routes, i.e. ones for which the backend will serve the frontend.
data FrontendRoute :: * -> * where
  FrontendRoute_Main :: FrontendRoute ()
  FrontendRoute_Channel :: FrontendRoute (PrimaryKey Channel Identity)
  FrontendRoute_Auth :: FrontendRoute (R SecureChannelResetRoute)

fullRouteEncoder
  :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
fullRouteEncoder = mkFullRouteEncoder
  (FullRoute_Backend BackendRoute_Missing :/ ())
  (\case
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
      BackendRoute_Api -> PathSegment "api" $ unitEncoder mempty
      BackendRoute_Status -> PathSegment "status" $ unitEncoder mempty
  )
  (\case
      FrontendRoute_Main -> PathEnd $ unitEncoder mempty
      FrontendRoute_Channel -> PathSegment "channel" $ pathOnlyEncoder . singletonListEncoder . unsafeTshowEncoder . isoEncoder _SqlSerial . isoEncoder _ChannelId
      FrontendRoute_Auth -> PathSegment "auth" $ pathComponentEncoder $ \case
        SecureChannelResetRoute_Email -> PathSegment "email" $ pathOnlyEncoder . singletonListEncoder . base64UriEncoder
  )

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  , ''SecureChannelResetRoute
  ]
