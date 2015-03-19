{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
import Prelude hiding (head)

import ReflexTalk.Example

import Reflex
import Reflex.Dom
import Reflex.ImpressJs
import Control.Monad
import Data.Default
import Data.Monoid
import Data.Function
import Data.Maybe
import qualified Data.Map as Map
import Data.Map (Map)
import Safe
import Language.Haskell.HsColour.InlineCSS
import Language.Haskell.HsColour.Colourise
import Text.RawString.QQ

main :: IO ()
main = do
  mainWidgetWithHead head body
  return ()

head = do
  let meta attrs = elAttr "meta" attrs $ return ()
      metaNameContent n c = meta ("name" =: n <> "content" =: c)
      stylesheet url = elAttr "link" ("href" =: url <> "rel" =: "stylesheet" <> "type" =: "text/css") $ return ()
  meta ("charset" =: "utf-8")
  metaNameContent "viewport" "width=1024"
  metaNameContent "apple-mobile-web-app-capable" "yes"
  el "title" $ text "Reflex ImpressJs Example"
  metaNameContent "description" "Reflex ImpressJs Example"
  metaNameContent "author" "Obsidian Systems LLC"
  stylesheet "http://fonts.googleapis.com/css?family=Alegreya"
  stylesheet "http://fonts.googleapis.com/css?family=Josefin+Sans:300,400"
  stylesheet "http://fonts.googleapis.com/css?family=Karma:400,300"
  stylesheet "css/css.css"
  -- <link rel="shortcut icon" href="favicon.png" />
  -- <link rel="apple-touch-icon" href="apple-touch-icon.png" /> 

ahref u t = elAttr "a" ("href" =: u) $ text t

body = do
  fallback $ el "p" $ do
    text "Sorry, your browser is not supported. A simplified version of the presentation follows, but to get the full experience, please use a recent version of Chrome, Firefox, or Safari, or contact "
    ahref "mailto:info@obsidian.systems" "info@obsidian.systems"
  impressDiv slides

--TODO: should we have doubleInput, too?
--TODO: move to reflex-dom
--TODO: Disallow bad inputs
integerInput :: MonadWidget t m => m (Dynamic t Integer)
integerInput = do
  x <- textInput
  mapDyn (fromMaybe 0 . readMay) $ _textInput_value x

--TODO: Tab key should only move between controls on the *current* slide

slides =
  [ slide Nothing "slide" (def { _x = 0 }) $ el "q" $ do
       el "h1" $ text "Reflex:"
       el "h2" $ text "Practical Functional Reactive Programming"
       el "h3" $ text "Ryan Trinkle"
       el "h4" $ text "Obsidian.Systems" --TODO: Use the words reflex and obsidian more
  , slide Nothing "slide" (def { _x = 1000 }) $ do
       $(example [r|
          do clicked <- button "Click me!"
             numClicks <- count clicked
             display numClicks
          |])
       return ()
  , slide Nothing "slide" (def { _x = 2000 }) $ do
       $(example [r|
          do x <- integerInput
             text "*"
             y <- integerInput
             text "="
             result <- combineDyn (*) x y
             display result
          |])
       return ()
  ]
