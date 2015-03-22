{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, CPP #-}

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

#ifdef __GHCJS__
#define JS(name, js, type) foreign import javascript unsafe js name :: type
#else
#define JS(name, js, type) name :: type ; name = undefined
#endif

JS(impressInit_, "impress().init()", IO ())

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

slideConfigToAttrs :: SlideConfig -> Map String String
slideConfigToAttrs (SlideConfig x y z scale rx ry rz) =
  Map.fromList [ ("data-x", show x)
               , ("data-y", show y)
               , ("data-z", show z)
               , ("data-scale", show scale)
               , ("data-rotate-x", show rx)
               , ("data-rotate-y", show ry)
               , ("data-rotate-z", show rz)
               ]
                                            
slide :: MonadWidget t m => Maybe String -> String -> SlideConfig -> m a -> m a
slide sid klass config content = elAttr "div" (maybe Map.empty ("id" =:) sid <> "class" =: ("step " <> klass) <> slideConfigToAttrs config) content
 
impressDiv :: MonadWidget t m => m () -> m ()
impressDiv slides = elAttr "div" ("id" =: "impress") $ do
  slides
  post <- getPostBuild
  performEvent_ $ fmap (const $ liftIO impressInit_) post
  return ()

fallback :: MonadWidget t m => m a -> m a
fallback c = divClass "fallback-message" c

