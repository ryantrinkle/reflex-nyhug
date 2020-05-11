{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Reflex.ImpressJs where

import Prelude hiding (head)
import Reflex
import Reflex.Dom
import Control.Monad
import Control.Monad.IO.Class
import Data.Default
import Data.Monoid
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Lens
import Data.Text (Text)
import qualified Data.Text as T
import Language.Javascript.JSaddle

-- JS(impressInit_, "impress().init()", IO ())

data SlideConfig
   = SlideConfig { _x :: Int
                 , _y :: Int
                 , _z :: Int
                 , _scale :: Int
                 , _rx :: Int
                 , _ry :: Int
                 , _rz :: Int
                 } deriving (Show, Read, Eq, Ord)

instance Default SlideConfig where
  def = SlideConfig 0 0 0 1 0 0 0

slideConfigToAttrs :: SlideConfig -> Map Text Text
slideConfigToAttrs (SlideConfig x y z scale rx ry rz) =
  Map.fromList [ ("data-x", T.pack $ show x)
               , ("data-y", T.pack $ show y)
               , ("data-z", T.pack $ show z)
               , ("data-scale", T.pack $ show scale)
               , ("data-rotate-x", T.pack $ show rx)
               , ("data-rotate-y", T.pack $ show ry)
               , ("data-rotate-z", T.pack $ show rz)
               ]

slide :: DomBuilder t m => Maybe Text -> Text -> SlideConfig -> m a -> m a
slide sid klass config content = elAttr "div" (maybe Map.empty ("id" =:) sid <> "class" =: ("step " <> klass) <> slideConfigToAttrs config) content

impressDiv :: _ => m a -> m a
impressDiv slides = elAttr "div" ("id" =: "impress" <> "data-width" =: "2048" <> "data-height" =: "1536") $ do
  s <- slides
  prerender_ blank $ do
    _ <- liftJSM $ global ^. js0 ("impress"::Text) ^. js0 ("init"::Text)
    pure ()
  return s

fallback :: _ => m a -> m a
fallback c = divClass "fallback-message" c

makeLenses ''SlideConfig
