{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Backend.View where

import Prelude hiding (id, (.), filter)

import Common.Api
import Common.Schema
import Control.Category
import Data.Functor.Const
import Data.Functor.Identity
import Database.Beam.Postgres
import Obelisk.Auth
import Obelisk.Auth.Bearer.Backend.Sign
import Obelisk.Auth.Bearer.Common.Sign
import Obelisk.View.Auth
import Obelisk.View.Beam
import Obelisk.View.Beam.Ordering
import Obelisk.View.Cartesian
import Obelisk.View.Convenience
import Obelisk.View.Fan
import Obelisk.View.BeamQuery.TableQ
import Obelisk.View.MultiMode
import Obelisk.View.Mux
import Obelisk.View.Table
import Obelisk.View.Iv.Class
import Obelisk.View.IdentityInterface
import Obelisk.View.DMap
import Obelisk.View.Intersection

import qualified Web.ClientSession as CS

import GHC.Generics

myView
  :: forall m
  .  ObeliskIvMode m
  => CS.Key
  -> ObeliskIv m (BeamDbInterface Postgres Db) (DMapInterface (MyViewKey (Signed AuthToken)) IdentityInterface)
myView csk = fanMuxIdentity $ \case
  MyViewKey_Public -> fanMuxIdentity $ \case
    MyViewKeyPublic_NumMessages ->
      cachedSqlCountTable . table (fieldToChain DbField1_Messages)
    MyViewKeyPublic_NumUsers ->
      cachedSqlCountTable . table (fieldToChain DbField1_Users)
  MyViewKey_Private -> (. addChannelCache) $ auth (authenticateToken csk) $ \(AuthToken (UserId myUid)) -> fanMuxIdentity $ \case
    MyViewKeyPrivate_MyChannels ->
      -- TODO: this would be better as an Outer join; so users can see the existence of channels they're not in.
      fromBeamTableInterface
      . filterBeamIv (Filter_Eq (fieldToChain UserIdField1_Id . fieldToChain (R1 ChannelMemberField1_User)) (Scalar myUid))
      . joinTables' (fieldToChain DbField1_Channels) (fieldToChain DbField1_ChannelMembers) (fieldToChain ChannelMemberIdField1_Channel)
      . fstIv @_ @(BeamDbInterface Postgres Db) @ChannelsInterface
    MyViewKeyPrivate_ChannelMessages cid -> --TODO: Distinguish between missing channels and empty channels
      --TODO: Restrict the user to only loading the channels they are a member of
      lookupMapIv cid . sndIv @_ @(BeamDbInterface Postgres Db) @ChannelsInterface
    MyViewKeyPrivate_User (UserId otherUid) ->
      fromBeamTableInterface .
      filterBeamIv (Filter_Eq (fieldToChain UserField1_Id) (Scalar otherUid)) .
      table (fieldToChain DbField1_Users) .
      fstIv @_ @_ @ChannelsInterface

type ChannelsInterface = MapInterface (PrimaryKey Channel Identity) (AppInterface TableInterface (LiftInterface Message))

-- addChannelCache :: (IvCategory iv , _) => iv (BeamDbInterface Postgres Db) (ProductInterface (BeamDbInterface Postgres Db) (MapInterface (PrimaryKey Channel Identity) (AppInterface TableInterface (LiftInterface Message))))
addChannelCache :: (Category iv, _) => iv (BeamDbInterface Postgres Db) (ProductInterface (BeamDbInterface Postgres Db) (MapInterface (PrimaryKey Channel Identity) (AppInterface TableInterface (LiftInterface Message))))
addChannelCache = id &&& mkChannelCache

-- mkChannelCache :: ( IvCategory iv , _) => iv (BeamDbInterface Postgres Db) (MapInterface (PrimaryKey Channel Identity) (AppInterface TableInterface (LiftInterface Message)))
mkChannelCache :: (Category iv, _) => iv (BeamDbInterface Postgres Db) (MapInterface (PrimaryKey Channel Identity) (AppInterface TableInterface (LiftInterface Message)))
mkChannelCache =
  muxMap (\(ChannelId cid) -> sqlSelectLimitTable orderBy 10 . filterBeamIv (filter cid)) .
  fan .
  table (fieldToChain DbField1_Messages) where
    filter cid = Filter_Eq tblField (Scalar cid) where
      tblField = fieldToChain ChannelIdField1_Id . fieldToChain MessageField1_Channel
    orderBy = OrderBy tblField $ Scalar $ Const Direction_Descending where
      tblField = fieldToChain MessageField1_Id
