{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Frontend where

import qualified GHCJS.DOM.Element as JS
import qualified Language.Javascript.JSaddle as JS

import qualified Data.Text as T
import Obelisk.Frontend
import Obelisk.Route

import Reflex.Dom.Core hiding (button, Window, fromJSString)

import Common.Api
import Common.Route

import Control.Applicative
import Control.Lens (_Left, _Right, (^?))
import Control.Monad
import Control.Monad.Reader
import Data.Aeson (ToJSON (..), FromJSON (..))
import qualified Data.Aeson as Aeson
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Data.Semigroup (First(..))
import Data.Text (Text)
import Data.Tuple (swap)
import Data.Void (Void, absurd)
import Data.These
import Data.GADT.Compare
import Data.Dependent.Sum (DSum)
import qualified Data.Dependent.Map as DMap
import Data.These.Lens
import GHC.Generics ((:*:)(..))

import ReflexTalk.Example

import Reflex
import Reflex.ImpressJs
import Reflex.TodoMVC
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Bitraversable
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Char
import Data.Default
import Data.Monoid
import Data.Function
import Data.Maybe
import qualified Data.Map as Map
import Data.Map (Map)
import GHCJS.Foreign
import GHCJS.Types
import Safe
import Language.Haskell.HsColour.InlineCSS
import Language.Haskell.HsColour.Colourise
import Text.RawString.QQ
import qualified Data.Text as T
import Data.Text.Encoding
import Network.URI
import Network.HTTP.Types.URI
import Data.Aeson (decode)
-- import Web.Twitter.Types hiding (Event)
import Control.Lens hiding ((&))
import GHCJS.DOM.HTMLElement
import GHCJS.DOM.HTMLIFrameElement
import GHCJS.DOM.Types hiding (Event, Text)
import GHCJS.DOM.XMLHttpRequest
import Language.Javascript.JSaddle
import Control.Monad.Ref
import Data.IORef
import Obelisk.Generated.Static

--TODO: Does Aeson still break down on Maybe (Maybe a)?

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = headWidget
  , _frontend_body = bodyWidget ""
  }

headWidget :: DomBuilder t m => m ()
headWidget = do
  let meta attrs = elAttr "meta" attrs $ return ()
      metaNameContent n c = meta ("name" =: n <> "content" =: c)
      stylesheet url = elAttr "link" ("href" =: url <> "rel" =: "stylesheet" <> "type" =: "text/css") $ return ()
  elAttr "script" ("src" =: static @"js/impress.js" <> "defer" =: "defer") blank
  meta ("charset" =: "utf-8")
  metaNameContent "viewport" "width=1024"
  metaNameContent "apple-mobile-web-app-capable" "yes"
  el "title" $ text "Reflex ImpressJs Example"
  metaNameContent "description" "Reflex ImpressJs Example"
  metaNameContent "author" "Obsidian Systems LLC"
  stylesheet "//fonts.googleapis.com/css?family=Alegreya"
  stylesheet "//fonts.googleapis.com/css?family=Raleway:400,300"
  stylesheet "//fonts.googleapis.com/css?family=Droid+Serif"
  stylesheet "//fonts.googleapis.com/css?family=Josefin+Sans:300,400"
  -- Telescope logo font
  elAttr "link" ("href" =: "http://fonts.googleapis.com/css?family=Coustard:900" <> "rel" =: "stylesheet" <> "type" =: "text/css") $ return ()
  stylesheet "//maxcdn.bootstrapcdn.com/font-awesome/4.3.0/css/font-awesome.min.css"
  stylesheet "//cdnjs.cloudflare.com/ajax/libs/skeleton/2.0.4/skeleton.min.css"
  stylesheet $ static @"css/hscolour-default.css"
  stylesheet $ static @"css/css.css"
  -- <link rel="shortcut icon" href="favicon.png" />
  -- <link rel="apple-touch-icon" href="apple-touch-icon.png" />

ahref u t = elAttr "a" ("href" =: u) $ text t

bodyWidget :: _ => Text -> m ()
bodyWidget rootURL = do
  fallback $ el "p" $ do
    text "Sorry, your browser is not supported. A simplified version of the presentation follows. To get the full experience, please use a recent version of Chrome, Firefox, or Safari, or contact "
    ahref "mailto:info@obsidian.systems" "info@obsidian.systems"
  rec ryanFooter creds tweets
      (creds, tweets) <- impressDiv $ slides rootURL
  return ()

--TODO: should we have doubleInput, too?
--TODO: move to reflex-dom
--TODO: Disallow bad inputs
integerInput :: _ => m (Dynamic t Integer)
integerInput = do
  x <- inputElement def
  mapDyn (fromMaybe 0 . readMay . T.unpack) $ value x

--TODO: Tab key should only move between controls on the *current* slide

buttonWithIcon :: _ => Text -> Text -> m (Event t ())
buttonWithIcon i t = do
  (e, _) <- el' "button" $ do
    icon i
    text $ " " <> t
  return $ _el_clicked e

slideWidth :: Int
slideWidth = 2500

slideHeight :: Int
slideHeight = 1500

type Status = T.Text --TODO

slides :: forall t m. _ => Text -> m (Dynamic t (Maybe Credential), Dynamic t [Status])
slides rootURL = do
  -- Part 1
  introSlides def
  (creds, tweets) <- twitterSlides rootURL $ def & y +~ slideHeight * 1
  reflexDemoSlides $ def & y +~ slideHeight * 2
  -- Break
  breakSlide $ def & y +~ slideHeight * 3
  -- Part 2
  frpRequirementsSlides $ def & y +~ slideHeight * 4
  reflexSemanticsSlides $ def & y +~ slideHeight * 5
  nextStepsSlides $ def & y +~ slideHeight * 6
  return (creds, tweets)

introSlides :: forall t m. _ => SlideConfig -> m ()
introSlides cfg = do
  slide Nothing "" (cfg & x +~ slideWidth * 0) $ do
    el "h1" $ text "Reflex"
    el "h4" $ text "Practical Functional Reactive Programming"
  slide Nothing "" (cfg & x +~ slideWidth * 1) $ do
    el "h3" $ do
      el "strong" $ text "Functional Reactive Programming"
      text " is a way of writing interactive software using only "
      el "strong" $ text "pure functions"
  slide Nothing "" (cfg & x +~ slideWidth * 2) $ do
    el "h3" $ do
      text "Pure functions enable "
      el "strong" $ text "equational"
      text " and "
      el "strong" $ text "local"
      text " reasoning"
  slide Nothing "" (cfg & x +~ slideWidth * 3) $ do
    el "h3" $ do
      el "strong" $ text "Equational reasoning"
      text " means that we can "
      el "strong" $ text "refactor"
      text " code safely"
    el "br" $ return ()
    el "h3" $ do
      text "x+x \x2261 2*x"
  slide Nothing "" (cfg & x +~ slideWidth * 4) $ do
    el "h3" $ do
      el "strong" $ text "Local reasoning"
      text " means that we can "
      el "strong" $ text "understand"
      text " code in isolation"
    el "br" $ return ()
    el "h3" $ do
      text "If f(x) = 2x, then f(3) \x2261 6"
  slide Nothing "" (cfg & x +~ slideWidth * 5) $ do
    el "h3" $ do
      el "strong" $ text "Equational"
      text " and "
      el "strong" $ text "local"
      text " reasoning make code "
      el "strong" $ text "maintainable"
      text " and "
      el "strong" $ text "reusable"
  slide Nothing "" (cfg & x +~ slideWidth * 6) $ do
    el "h3" $ do
      text "This makes Haskell amazing for "
      el "strong" $ text "batch systems,"
      text " like compilers and web APIs, which mostly consume input at the beginning and produce output at the end"
  slide Nothing "" (cfg & x +~ slideWidth * 7) $ do
    el "h3" $ do
      text "FRP brings the power of pure functions to "
      el "strong" $ text "interactive systems,"
      text " such as web pages and audio synthesizers, which consume input and produce output over time"
  let reflexStart = 8
  slide Nothing "" (cfg & x +~ slideWidth * (reflexStart + 0)) $ do
    el "h3" $ do
      el "strong" $ text "Reflex"
      text " is a "
      el "strong" $ text "semantics"
      text " for FRP and an "
      el "strong" $ text "implementation"
      text " of that semantics"
  slide Nothing "" (cfg & x +~ slideWidth * (reflexStart + 1)) $ do
    el "h3" $ do
      text "Reflex works with "
      el "strong" $ text "GHC"
      text " and "
      el "strong" $ text "GHCJS"
  slide Nothing "" (cfg & x +~ slideWidth * (reflexStart + 2)) $ do
    el "h3" $ do
      text "The flagship Reflex toolkit is "
      el "strong" $ text "Reflex.Dom"
      text $ ": a library for building cutting edge web apps"

-- htmlElementCreateShadowRoot (HTMLElement e) = fmap HTMLElement $ e ^. js0 "createShadowRoot"

reflexDemoSlides :: forall t m. _ => SlideConfig -> m ()
reflexDemoSlides cfg = do
  slide Nothing "" (cfg & x +~ slideWidth * 0) $ do
    el "h1" $ text "Made with Reflex"
  slide Nothing "" (cfg & x +~ slideWidth * 1) $ do
    el "h1" $ text "reflex-todomvc"
    elAttr "div" ("style" =: "width:1920px;height:1200px;") $ do
      {-
      e <- buildEmptyElement "div" ("class" =: "hidden-scroll" <> "style" =: "width:960px;height:600px;transform-origin: 0 0 0;transform:scale(2,2);overflow:auto" :: Map Text Text)
      eShadowRoot <- liftIO $ htmlElementCreateShadowRoot e
      subWidget (toNode eShadowRoot) $ do
        el "head" $ do
          el "style" $ text "@import \"todomvc/css.css\""
        el "body" $ do
          todoMVC
      -}
      text "This area is missing"
  slide Nothing "" (cfg & x +~ slideWidth * 2) $ do
    elClass "h1" "redline" $ do
      elAttr "span" ("style" =: "color:red;") $ text "RED"
      text "LINE"
    el "p" $ text "Online document negotiation and collaboration"
    elAttr "img" ("src" =: "images/redline.png" <> "class" =: "screenshot") $ return ()
  slide Nothing "" (cfg & x +~ slideWidth * 3) $ do
    elClass "h1" "prasava" $ do
      text "Prasava"
    el "p" $ text "Real-time legal courier geolocation tracking"
    elAttr "img" ("src" =: "images/prasava.png" <> "class" =: "screenshot") $ return ()

  slide Nothing "" (cfg & x +~ slideWidth * 4) $ do
    elAttr "h1" ("style" =: "font-family:'Coustard',serif;font-weight:900") $ text "Telescope"
    el "p" $ text "Powerful and flexible data analysis tools"
    elAttr "img" ("src" =: "images/telescope.png" <> "class" =: "screenshot") $ return ()
  slide Nothing "" (cfg & x +~ slideWidth * 5) $ do
    elClass "h1" "obsidian" $ logo ""
    el "p" $ text "https://obsidian.systems"
    elAttr "img" ("src" =: "images/obsidian.png" <> "class" =: "screenshot") $ return ()

  slide (Just "overview") "no-pointer-events" (cfg & x +~ slideWidth * 4
                        & y -~ (slideHeight `div` 5)
                        & scale *~ 11
                   ) $ do
    el "h1" $ text "This presentation"
    el "h4" $ text "Built with Reflex.Dom, impress.js, and Snap"
    elAttr "div" ("style" =: "width:0;height:1050px") $ return ()

breakSlide :: forall t m. _ => SlideConfig -> m ()
breakSlide cfg = slide Nothing "" cfg $ do
  el "h1" $ text "Try it out!"
  elAttr "pre" ("style" =: "font-size:larger") $ text $ T.pack $ trimLeading [r|
      git clone https://github.com/ryantrinkle/try-reflex
      cd try-reflex
      ./try-reflex
    |]

frpRequirementsSlides :: forall t m. _ => SlideConfig -> m ()
frpRequirementsSlides cfg = do
  slide Nothing "" (cfg & x +~ slideWidth * 0) $ do
    el "h3" $ do
      text "Practical systems must be "
      el "strong" $ text "expressive,"
      text " "
      el "strong" $ text "comprehensible,"
      text " and "
      el "strong" $ text "efficient"
  slide Nothing "" (cfg & x +~ slideWidth * 1) $ do
    el "h3" $ do
      text "Practical FRP should support "
      el "strong" $ text "dynamic data flow"
  slide Nothing "" (cfg & x +~ slideWidth * 2) $ do
    el "h3" $ do
      text "In Reflex.Dom, widgets can be created, laid out, or destroyed "
      el "strong" $ text "within"
      text " the FRP semantics"
  slide Nothing "" (cfg & x +~ slideWidth * 3) $ do
    el "h3" $ do
      text "Practical FRP should support a "
      el "strong" $ text "variety of concepts of time"
  slide Nothing "" (cfg & x +~ slideWidth * 4) $ do
    el "h3" $ do
      text "Reflex only requires that time is "
      el "strong" $ text "ordered"
      text $ T.pack $ '\x2014' : "both continuous and discrete time are possible"
  slide Nothing "" (cfg & x +~ slideWidth * 5) $ do
    el "h3" $ do
      text "Practical FRP should be "
      el "strong" $ text "fully deterministic"
  slide Nothing "" (cfg & x +~ slideWidth * 6) $ do
    el "h3" $ do
      text "All Reflex primitives have "
      el "strong" $ text "well-defined,"
      text " implementation-independent behavior"
  slide Nothing "" (cfg & x +~ slideWidth * 7) $ do
    el "h3" $ do
      text "Practical FRP should use "
      el "strong" $ text "idiomatic Haskell"
  slide Nothing "" (cfg & x +~ slideWidth * 8) $ do
    el "h3" $ do
      text "Reflex has 10 primitives; "
      el "strong" $ text "8 are pure"
      text " and 2 are monadic"
  slide Nothing "" (cfg & x +~ slideWidth * 9) $ do
    el "h3" $ do
      text "Practical FRP should have "
      el "strong" $ text "good performance"
  slide Nothing "" (cfg & x +~ slideWidth * 10) $ do
    el "h3" $ do
      text "In Reflex, overall application size has "
      el "strong" $ text "no"
      text " impact on event propagation time"
  slide Nothing "" (cfg & x +~ slideWidth * 11) $ do
    el "h3" $ do
      text "Practical FRP should be "
      el "strong" $ text "fully garbage-collected"
  slide Nothing "" (cfg & x +~ slideWidth * 12) $ do
    el "h3" $ do
      text "Reflex uses automatic memory management for "
      el "strong" $ text "all"
      text " datastructures, even external callbacks"

reflexSemanticsSlides :: forall t m. _ => SlideConfig -> m ()
reflexSemanticsSlides cfg = do
  slide Nothing "" (cfg & x +~ slideWidth * 0) $ do
    el "h1" $ text "Types"
    examplePre [r|
      Event t a

      Behavior t a
    |] mempty
  slide Nothing "" (cfg & x +~ slideWidth * 1) $ do
    el "h1" $ text "Trivial constructors"
    examplePre [r|
      never :: Event t a

      constant :: a -> Behavior t a
    |] mempty
  slide Nothing "" (cfg & x +~ slideWidth * 2) $ do
    el "h1" $ text "Mapping functions"
    examplePre [r|
      push :: (a -> PushM t (Maybe b)) -> Event t a -> Event t b

      pull :: PullM t a -> Behavior t a
    |] mempty
  slide Nothing "" (cfg & x +~ slideWidth * 3) $ do
    el "h1" $ text "Event distribution"
    examplePre [r|
      merge :: GCompare k => DMap (WrapArg (Event t) k) -> Event t (DMap k)

      fan :: GCompare k => Event t (DMap k) -> EventSelector t k
      select :: EventSelector t k -> k a -> Event t a
    |] mempty
  slide Nothing "" (cfg & x +~ slideWidth * 4) $ do
    el "h1" $ text "Higher-order combinators"
    examplePre [r|
      switch :: Behavior t (Event t a) -> Event t a

      coincidence :: Event t (Event t a) -> Event t a
    |] mempty
  slide Nothing "" (cfg & x +~ slideWidth * 5) $ do
    el "h1" $ text "Time-sensitive"
    examplePre [r|
      sample :: MonadSample t m => Behavior t a -> m a
      instance MonadSample t (PullM t)
      instance MonadSample t (PushM t)

      hold :: MonadHold t m => a -> Event t a -> m (Behavior t a)
      instance MonadHold t (PushM t)
    |] mempty

nextStepsSlides :: forall t m. _ => SlideConfig -> m ()
nextStepsSlides cfg = do
  slide Nothing "" (cfg & x +~ slideWidth * 0) $ do
    el "h1" $ text "The Future of Reflex"
  slide Nothing "" (cfg & x +~ slideWidth * 1) $ el "h3" $ do
    text "Reflex is ready for production use "
    el "strong" $ text "today"
  slide Nothing "" (cfg & x +~ slideWidth * 2) $ el "h3" $ do
    text "The Reflex API is improving rapidly, so please "
    el "strong" $ text "use upper-bounds"
  slide Nothing "" (cfg & x +~ slideWidth * 3) $ do
    el "h3" $ do
      text "Reflex is available on "
      el "strong" $ text "github"
      text " and "
      el "strong" $ text "hackage"
    elAttr "pre" ("style" =: "font-size:larger") $ do
      text "https://github.com/ryantrinkle/reflex\n"
      text "https://hackage.haskell.org/package/reflex"
  slide Nothing "" (cfg & x +~ slideWidth * 4) $ do
    el "h1" $ text "Questions?"


reflexTypes :: Map String String
reflexTypes = Map.fromList [ ("constDyn", "a -> Dynamic t a")
                           , ("tag", "Behavior t a -> Event t b -> Event t a")
                           , ("current", "Dynamic t a -> Behavior t a")
                           , ("dynText", "Dynamic t Text -> m ()")
                           , ("holdDyn", "a -> Event t a -> Dynamic t a")
                           , ("textAreaElement", "TextAreaElementConfig t -> m (TextAreaElement t)")
                           , ("mapDyn", "(a -> b) -> Dynamic t a -> m (Dynamic t b)")
                           , ("display", "(Show a) => Dynamic t a -> m ()")
                           , ("text", "Text -> m ()")
                           , ("fmap", "(Functor f) => (a -> b) -> f a -> f b")
                           , ("el", "_ => Text -> m a -> m a")
                           , ("ffilter", "(FunctorMaybe f) => (a -> Bool) -> f a -> f a")
                           , ("foldDyn", "(a -> b -> b) -> b -> Event t a -> m (Dynamic t b)")
                           , ("forDyn", "Dynamic t a -> (a -> b) -> m (Dynamic t b)")
                           , ("attachDyn", "Dynamic t a -> Event t b -> Event t (a, b)")
                           , ("fmapMaybe", "(FunctorMaybe f) => (a -> Maybe b) -> f a -> f a")
                           , ("performRequestAsync", "Event t XhrRequest -> m (Event t XhrResponse)")
                           , ("decodeXhrResponse", "(FromJSON a) => XhrResponse -> Maybe a")
                           , ("simpleList", "Dynamic t [v] -> (Dynamic t v -> m a) -> m (Dynamic t [a])")
                           , ("updated", "Dynamic t a -> Event t a")
                           , ("divClass", "Text -> m a -> m a")
                           , ("elClass", "Text -> Text -> m a -> m a")
                           ]

-- union is biased toward reflexTypes
withReflexTypes = Map.union reflexTypes

twitterSlides
  :: forall (t :: *) (m :: * -> *) js
  .  ( Ref m ~ IORef
     , Ref (Performable m) ~ IORef
     , Monad m
     , DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     , TriggerEvent t m
     , Prerender js t m
     , PerformEvent t m
     )
  => Text
  -> SlideConfig
  -> m (Dynamic t (Maybe Credential), Dynamic t [Status])
twitterSlides rootURL cfg = do
  let twitterSlideTypes = withReflexTypes $ Map.fromList
        [ ("newTweet", "Event t Text")
        , ("tweetBox", "TextAreaElement t")
        , ("tweetButton", "Event t ()")
        , ("latestTweet", "Dynamic t Text")
        , ("tweetHistory", "Dynamic t [Text]")
        , ("numChars", "Dynamic t Int")
        , ("length", "[a] -> Int")
        , ("displayNumChars", "TextAreaElement t -> m ()")
        , ("twitterAuthorizeButton", "m (Dynamic t (Maybe Credential))")
        , ("creds", "Dynamic t (Maybe Credential)")
        , ("tweetBoxAttrs", "Dynamic t (Map Text Text)")
        , ("tweetWidget", "Dynamic t (Map Text Text) -> m (Event t Text)")
        , ("toTweetReq", "Event t (Maybe Credential) -> Event t Text -> Event t (Maybe XhrRequest)")
        , ("tweetReq", "Event t XhrRequest")
        , ("tweeted", "Event t XhrResponse")
        , ("newLiveTweet", "Event t (Maybe Text)")
        , ("liveTweetWidget", "Dynamic t (Maybe Credential) -> m (Event t (Maybe Text)")
        , ("tweetStream", "Event t Credential -> m (Event t Status)")
        , ("buttonWithIcon", "Text -> Text -> m (Event t ())")
        , ("tweets", "Dynamic t [Status]")
        , ("startStream", "Event t Credential -> m (Event t Status)")
        , ("tweetStatus", "Status -> Text")
        , ("tweetUserName", "Status -> Text")
        ]
  slide Nothing "" (cfg & x +~ slideWidth * 0) $ do
    el "h1" $ text "#reflexFRP"
    el "h2" $ text "Building a Twitter frontend"
  slide Nothing "" (cfg & x +~ slideWidth * 1) $ do
    el "h4" $ text "An input and its value"
    examplePre [r|
       do tweetBox <- textAreaElement def
          dynText $ value tweetBox
      |] twitterSlideTypes
    do tweetBox <- textAreaElement def
       dynText $ value tweetBox
    return ()
  slide Nothing "" (cfg & x +~ slideWidth * 2) $ do
    el "h4" $ text "Adding an attribute"
    examplePre [r|
       do tweetBox <- textAreaElement $
            def & initialAttributes .~ ("maxlength" =: "140")
          dynText $ value tweetBox
      |] twitterSlideTypes
    do tweetBox <- textAreaElement $
         def & initialAttributes .~ ("maxlength" =: "140")
       dynText $ value tweetBox
    return ()
  slide Nothing "" (cfg & x +~ slideWidth * 3) $ do
    el "h4" $ text "Displaying the length of the input"
    examplePre [r|
       do tweetBox <- textAreaElement $
            def & initialAttributes .~ ("maxlength" =: "140")
          el "div" $ do
            numChars <- mapDyn length $ value tweetBox
            display numChars
            text " characters"
      |] twitterSlideTypes
    do tweetBox <- textAreaElement $
         def & initialAttributes .~ "maxlength" =: "140"
       el "div" $ do
         numChars <- mapDyn T.length $ value tweetBox
         display numChars
         text " characters"
    return ()
  slide Nothing "" (cfg & x +~ slideWidth * 4) $ do
    el "h4" $ text "Showing a value when an event occurs"
    examplePre [r|
      do newTweet <- el "div" $ do
           tweetBox <- textAreaElement $
             def & initialAttributes .~ ("maxlength" =: "140")
           tweetButton <- buttonWithIcon "twitter" "Tweet!"
           displayNumChars tweetBox
           return $ tag (current (value tweetBox)) tweetButton
         el "div" $ do
           latestTweet <- holdDyn "" newTweet
           text "Last status: "
           dynText latestTweet
    |] twitterSlideTypes
    do newTweet <- el "div" $ do
         tweetBox <- textAreaElement $
           def & initialAttributes .~ ("maxlength" =: "140")
         tweetButton <- buttonWithIcon "twitter" "Tweet!"
         displayNumChars tweetBox
         return $ tag (current (value tweetBox)) tweetButton
       el "div" $ do
         latestTweet <- holdDyn "" newTweet
         text "Last status: "
         dynText latestTweet
  slide Nothing "" (cfg & x +~ slideWidth * 5) $ do
    el "h4" $ text "Clearing the input after tweeting"
    examplePre [r|
      do newTweet <- el "div" $ do
           rec tweetBox <- textAreaElement $
                 def & initialAttributes .~ ("maxlength" =: "140")
                     & setValue .~ fmap (\_ -> "") tweetButton
               tweetButton <- buttonWithIcon "twitter" "Tweet!"
           displayNumChars tweetBox
           return $ tag (current (value tweetBox)) tweetButton
         el "div" $ do
           latestTweet <- holdDyn "" newTweet
           text "Last status: "
           dynText latestTweet
    |] twitterSlideTypes
    do newTweet <- el "div" $ do
         rec tweetBox <- textAreaElement $
               def & initialAttributes .~ ("maxlength" =: "140")
                   & setValue .~ fmap (\_ -> "") tweetButton
             tweetButton <- buttonWithIcon "twitter" "Tweet!"
         displayNumChars tweetBox
         return $ tag (current (value tweetBox)) tweetButton
       el "div" $ do
         latestTweet <- holdDyn "" newTweet
         text "Last status: "
         dynText latestTweet
  slide Nothing "" (cfg & x +~ slideWidth * 6) $ do
    el "h4" $ text "Disallowing empty tweets"
    examplePre [r|
      do newTweet <- el "div" $ do
           rec tweetBox <- textAreaElement $
                 def & initialAttributes .~ ("maxlength" =: "140")
                     & setValue .~ fmap (\_ -> "") tweetButton
               tweetButton <- buttonWithIcon "twitter" "Tweet!"
           displayNumChars tweetBox
           return $ ffilter (/="") $ tag (current (value tweetBox)) tweetButton
         el "div" $ do
           latestTweet <- holdDyn "" newTweet
           text "Last status: "
           dynText latestTweet
    |] twitterSlideTypes
    do newTweet <- el "div" $ do
         rec tweetBox <- textAreaElement $
               def & initialAttributes .~ ("maxlength" =: "140")
                   & setValue .~ fmap (\_ -> "") tweetButton
             tweetButton <- buttonWithIcon "twitter" "Tweet!"
         displayNumChars tweetBox
         return $ ffilter (/="") $ tag (current (value tweetBox)) tweetButton
       el "div" $ do
         latestTweet <- holdDyn "" newTweet
         text "Last status: "
         dynText latestTweet
  slide Nothing "" (cfg & x +~ slideWidth * 7) $ do
    el "h4" $ text "Building up a list of tweets"
    examplePre [r|
      do newTweet <- el "div" $ do
           rec tweetBox <- textAreaElement $
                 def & initialAttributes .~ ("maxlength" =: "140")
                     & setValue .~ fmap (\_ -> "") tweetButton
               tweetButton <- buttonWithIcon "twitter" "Tweet!"
           displayNumChars tweetBox
           return $ ffilter (/="") $ tag (current (value tweetBox)) tweetButton
         el "div" $ do
           tweetHistory <- foldDyn (:) [] newTweet
           text "Tweet history: "
           display tweetHistory
    |] twitterSlideTypes
    do newTweet <- el "div" $ do
         rec tweetBox <- textAreaElement $
               def & initialAttributes .~ ("maxlength" =: "140")
                   & setValue .~ fmap (\_ -> "") tweetButton
             tweetButton <- buttonWithIcon "twitter" "Tweet!"
         displayNumChars tweetBox
         return $ ffilter (/="") $ tag (current (value tweetBox)) tweetButton
       el "div" $ do
         tweetHistory <- foldDyn (:) [] newTweet
         text "Tweet history: "
         display tweetHistory
  creds <- slide Nothing "" (cfg & x +~ slideWidth * 8) $ do
    let twitterAuthorizeButton = twitterAuthorize rootURL
    el "h4" $ text "Authorizing Twitter"
    examplePre [r|
      do creds <- twitterAuthorizeButton
         newTweet <- el "div" $ do
           disableUntilAuth <- forDyn creds $ \c ->
             if isNothing c then ("disabled" =: "true") else Map.empty
           tweetBoxAttrs <- dynamicAttributesToModifyAttributes $ fmap (Map.insert "maxlength" "140") disableUntilAuth
           rec tweetBox <- textAreaElement $
                 def & modifyAttributes .~ tweetBoxAttrs
                     & setValue .~ fmap (const "") tweetButton
               tweetButton <- buttonWithIcon "twitter" "Tweet!"
           displayNumChars tweetBox
           return $ ffilter (/="") $ tag (current (value tweetBox)) tweetButton
       |] twitterSlideTypes
    do creds <- twitterAuthorizeButton
       newTweet <- el "div" $ do
         disableUntilAuth <- forDyn creds $ \c ->
           if isNothing c then ("disabled" =: "true") else Map.empty
         tweetBoxAttrs <- dynamicAttributesToModifyAttributes $ fmap (Map.insert "maxlength" "140") disableUntilAuth
         rec tweetBox <- textAreaElement $
               def & modifyAttributes .~ tweetBoxAttrs
                   & setValue .~ fmap (const "") tweetButton
             tweetButton <- buttonWithIcon "twitter" "Tweet!"
         displayNumChars tweetBox
         return $ ffilter (/="") $ tag (current (value tweetBox)) tweetButton
       return creds
  slide Nothing "" (cfg & x +~ slideWidth * 9) $ do
    el "h4" $ text "Making a tweet request"
    examplePre [r|
      do newTweet <- tweetWidget creds
         let tweetReq = fmapMaybe (uncurry toTweetReq) $ attachDyn creds newTweet
         tweeted <- performRequestAsync tweetReq
         latestTweet <- holdDyn Nothing $ fmap decodeXhrResponse tweeted
         text "Last status: "
         dynText =<< mapDyn (maybe "" tweetStatus) latestTweet
     |] twitterSlideTypes
    do newTweet <- tweetWidget creds
       -- let tweetReq = fmapMaybe (uncurry toTweetReq) $ attachDyn creds newTweet
       -- tweeted <- performRequestAsync tweetReq
       -- latestTweet <- holdDyn Nothing $ fmap decodeXhrResponse tweeted
       let latestTweet = pure Nothing --TODO
       text "Last status: "
       dynText =<< mapDyn (maybe "" tweetStatus) latestTweet
       return ()
  tweets <- slide Nothing "" (cfg & x +~ slideWidth * 10) $ divClass "left" $ do
    el "h4" $ text "Showing a stream of tweets"
    examplePre [r|
      do _ <- liveTweetWidget creds
         tweetStream <- startStream $ fmapMaybe id $ updated creds
         tweets <- foldDyn (:) [] tweetStream
         divClass "stream" $ simpleList tweets $ \t -> el "div" $ do
           el "strong" $ dynText =<< mapDyn tweetUserName t
           text ": "
           dynText =<< mapDyn tweetStatus t
     |] twitterSlideTypes
    do -- _ <- liveTweetWidget creds
       -- tweetStream <- startStream $ fmapMaybe id $ updated creds
       -- tweets <- foldDyn (:) [] tweetStream
       let tweets = pure [] --TODO
       divClass "stream" $ simpleList tweets $ \t -> el "div" $ do
         el "strong" $ dynText =<< mapDyn tweetUserName t
         text ": "
         dynText =<< mapDyn tweetStatus t
       return tweets
  slide Nothing "" (cfg & x +~ slideWidth * 11) $ divClass "left" $ do
    el "h4" $ text "Streaming with style"
    examplePre [r|
      do _ <- liveTweetWidget creds
         divClass "stream" $ elClass "ul" "fa-ul" $ do
           simpleList tweets $ \t -> el "li" $ do
             elClass "i" "fa-li fa fa-twitter" $ return ()
             el "strong" $ dynText =<< mapDyn tweetUserName t
             el "p" $ dynText =<< mapDyn tweetStatus t
      |] twitterSlideTypes
    do -- _ <- liveTweetWidget creds --TODO
       divClass "stream" $ elClass "ul" "fa-ul" $ do
         el "h1" $ dynText =<< mapDyn userNameFromCreds creds
         simpleList tweets $ \t -> el "li" $ do
           elClass "i" "fa-li fa fa-twitter" $ return ()
           el "strong" $ dynText =<< mapDyn tweetUserName t
           el "p" $ dynText =<< mapDyn tweetStatus t
       return ()
  return (creds, tweets)

data StreamingAPI = SStatus Status

onlyStatus :: StreamingAPI -> Maybe Status
onlyStatus s = case s of
                    SStatus a -> Just a
                    _ -> Nothing

userNameFromCreds :: Maybe Credential -> Text
userNameFromCreds c = case join $ fmap (lookup (encodeUtf8 $ T.pack "screen_name")) c of
                            Nothing -> "Please login"
                            Just c' -> "@" <> (decodeUtf8 c')


tweetUserName = const "tweetUserName" --TODO: userName . statusUser
tweetStatus = const "tweetStatus" --TODO: statusText

toTweetReq :: Maybe Credential -> Text -> Maybe (XhrRequest Text)
toTweetReq mc s = case mc of
                       Just c -> Just $ XhrRequest "POST" "/twitter/status" $ def { _xhrRequestConfig_sendData = T.pack $ show (c,s) }
                       Nothing -> Nothing

liveTweetWidget :: _ => Dynamic t (Maybe Credential) -> m (Event t (Maybe Text))
liveTweetWidget creds = do
  newTweet <- tweetWidget creds
  let tweetReq = fmapMaybe (uncurry toTweetReq) $ attachDyn creds newTweet
  liftM (fmap decodeXhrResponse) $ performRequestAsync tweetReq

tweetWidget :: (DomBuilder t m, PostBuild t m, MonadFix m) => Dynamic t (Maybe Credential) -> m (Event t Text)
tweetWidget credentials = el "div" $ do
  disableUntilAuth <- forDyn credentials $ \c ->
    if isNothing c then ("disabled" =: "true") else Map.empty
  tweetBoxAttrs <- dynamicAttributesToModifyAttributes $ fmap (Map.insert "maxlength" "140") disableUntilAuth
  rec tweetBox <- textAreaElement $
        def & modifyAttributes .~ tweetBoxAttrs
            & setValue .~ fmap (const "") tweetButton
      tweetButton <- buttonWithIcon "twitter" "Tweet!"
  displayNumChars tweetBox
  return $ ffilter (/="") $ tag (current (value tweetBox)) tweetButton

type Credential = SimpleQuery

twitterAuthorize :: forall t m. _ => Text -> m (Dynamic t (Maybe Credential))
twitterAuthorize rootURL = pure $ pure Nothing {- prerender (pure (pure Nothing)) $ divClass "twitter-authorize" $ do
  r <- performRequestAsync . fmap (const $ XhrRequest "GET" ("/oauth?callback=" <> rootURL <> "/blank") def) =<< getPostBuild
  url <- holdDyn "" $ fmapMaybe id $ fmap respBody r
  auth <- buttonWithIcon "key" "Authorize"
  --TODO
  --win <- performEvent (fmap (\u -> liftIO $ windowOpen u "twitter-oauth" "height=250, width=250") $ tagDyn url auth)
  --temp <- performEventAsync (fmap (\w cb -> liftIO $ waitForOauth w cb) win)
  --cr <- performRequestAsync $ fmap (\tc -> XhrRequest "GET" ("/twitter/secret" <> toQueryString tc) def) temp
  --let c :: Event t Credential = fmapMaybe readMay $ fmapMaybe respBody cr
  let c = never
  creds <- holdDyn Nothing $ fmap Just c
  let loading = fmap (const $ icon "spinner fa-pulse") auth
      check = fmap (const $ icon "check") (updated creds)
  dyn =<< holdDyn blank (leftmost [loading, check])
  return creds -}

displayNumChars :: _ => TextAreaElement r s t -> m ()
displayNumChars tweetBox = el "div" $ do
  numChars <- mapDyn T.length $ value tweetBox
  display numChars
  text " characters"

tweetList :: (_) => Dynamic t [Status] -> m ()
tweetList tweets = do
  divClass "stream" $ elClass "ul" "fa-ul" $ do
    simpleList tweets $ \t -> el "li" $ do
      elClass "i" "fa-li fa fa-twitter" $ return ()
      el "strong" $ dynText =<< mapDyn tweetUserName t
      el "p" $ dynText =<< mapDyn tweetStatus t
  return ()

startStream :: forall t m. _ => Event t Credential -> m (Event t Status)
startStream cred = do
  s <- liftM (switch . current) $ widgetHold (return never) (fmap stream cred)
  return $ fmapMaybe onlyStatus s


stream :: _ => Credential -> m (Event t StreamingAPI)
stream c = do
  pure never --TODO
  --ws <- webSocket $ "/twitter/userStream" <> toQueryString c
  --return $ fmapMaybe id $ fmap (Aeson.decode . LBS.fromStrict) ws

icon i = elClass "i" ("fa fa-" <> i) $ return ()

biseqFirst :: Monad m => (m a, b) -> m (a, b)
biseqFirst (a,b) = bisequence (a, return b)

respBody :: XhrResponse -> Maybe Text
respBody = _xhrResponse_body

toQueryString :: Credential -> Text
toQueryString = decodeUtf8 . renderSimpleQuery True

waitForOauth :: Window -> (Credential -> IO ()) -> IO ()
waitForOauth w cb = void $ forkIO $ do
  let go = do
        l <- pure $ Just "" --TODO: windowLocationHref w
        case l of
             Just l' | l' /= "about:blank"
                     , Just uri <- parseURI l'
                     -> do
                          --TODO: windowClose w
                          cb $ parseSimpleQuery $ encodeUtf8 $ T.pack $ uriQuery uri
             _ -> do
               threadDelay 250000
               go
  go

lightenPunctuation :: _ => Text -> m ()
lightenPunctuation s = do
  let (a,b) = T.span Data.Char.isAlphaNum s
  el "span" $ text a
  case T.splitAt 1 b of
       ("", _) -> return ()
       (x, xs) -> do
         elAttr "span" ("class" =: "light") $ text x
         lightenPunctuation xs

logo :: _ => Text -> m ()
logo k = elAttr "span" ("class" =: ("logo " <> k)) $ withPunct "OBSIDIAN" "." "SYSTEMS"

withPunct :: _ => Text -> Text -> Text -> m ()
withPunct a p b = do
  text $ T.take (T.length a - 1) a
  elAttr "span" ("class" =: "") $ text $ T.takeEnd 1 a
  elAttr "span" ("class" =: "punctuation") $ text p
  text b

ryanFooter :: _ => Dynamic t (Maybe Credential) -> Dynamic t [Status] -> m ()
ryanFooter creds tweets = do
  t <- mapDyn headMay tweets
  showHide <- mapDyn (\x -> if isNothing x then Map.empty else ("class" =: "fa-li fa fa-twitter")) t
  divClass "tweet-footer" $ do
    elDynAttr "i" showHide $ return ()
    el "strong" $ dynText =<< mapDyn (maybe "" id . fmap tweetUserName) t
    text ": "
    el "span" $ dynText =<< mapDyn (maybe "" id . fmap tweetStatus) t
  elAttr "a" ("class" =: "logo logo-bottom" <> "href" =: "mailto:ryan.trinkle@obsidian.systems")  $ do
    withPunct "RYAN" "." "TRINKLE"
    withPunct "" "@" ""
    withPunct "OBSIDIAN" "." "SYSTEMS"

--------------------------------------------------------------------------------
-- Old deprecated functions
--------------------------------------------------------------------------------

tagDyn :: Reflex t => Dynamic t a -> Event t b -> Event t a
tagDyn = tagPromptlyDyn

mapDyn :: (Reflex t, Applicative m) => (a -> b) -> Dynamic t a -> m (Dynamic t b)
mapDyn f = pure . fmap f

forDyn :: (Reflex t, Applicative m) => Dynamic t a -> (a -> b) -> m (Dynamic t b)
forDyn = flip mapDyn

attachDyn :: Reflex t => Dynamic t a -> Event t b -> Event t (a, b)
attachDyn = attachPromptlyDyn

instance Reflex t => HasSetValue (TextAreaElementConfig r t s) where
  type SetValue (TextAreaElementConfig r t s) = Event t Text
  setValue = textAreaElementConfig_setValue
