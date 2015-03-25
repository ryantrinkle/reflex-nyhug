{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, QuasiQuotes, JavaScriptFFI, CPP, ForeignFunctionInterface, RecursiveDo #-}
import Prelude hiding (head)

import ReflexTalk.Example

import Reflex
import Reflex.Dom
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
import Web.Twitter.Types hiding (Event)
import Control.Lens hiding ((&))
import GHCJS.DOM.HTMLElement
import GHCJS.DOM.HTMLIFrameElement
import GHCJS.DOM.Types hiding (Event)
import GHCJS.DOM.XMLHttpRequest
import WebSocket


-- Note: The C preprocessor will fail if you use a single-quote in the name
#ifdef __GHCJS__
#define JS(name, js, type) foreign import javascript unsafe js name :: type
#else
#define JS(name, js, type) name :: type ; name = undefined
#endif

JS(globalLocationOrigin_, "location.origin", IO JSString)

globalLocationOrigin :: IO String
globalLocationOrigin = liftM fromJSString globalLocationOrigin_

newtype Window = Window { unWindow :: JSRef Window }

JS(windowOpen_, "[window.open($1, $2, $3, false)]", JSString -> JSString -> JSString -> IO (JSRef Window))
JS(windowClose_, "$1[0].close()", JSRef Window -> IO ())
JS(windowLocationHref_, "try { $r = $1[0].location.href } catch (a) { $r = null }", JSRef Window -> IO JSString)

windowOpen :: String -> String -> String -> IO Window
windowOpen url target specs = do
  w <- liftIO $ windowOpen_ (toJSString url) (toJSString target) (toJSString specs)
  return $ Window w

windowLocationHref :: Window -> IO (Maybe String)
windowLocationHref w = do
  l <- liftIO $ windowLocationHref_ $ unWindow w
  if isNull l
     then return Nothing
     else return $ Just $ fromJSString l

windowClose :: Window -> IO ()
windowClose = windowClose_ . unWindow

main :: IO ()
main = do
  rootURL <- globalLocationOrigin
  mainWidgetWithHead head $ body rootURL
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
  stylesheet "//fonts.googleapis.com/css?family=Alegreya"
  --stylesheet "//fonts.googleapis.com/css?family=Droid+Serif"
  stylesheet "//fonts.googleapis.com/css?family=Raleway:400,300"
  --stylesheet "//fonts.googleapis.com/css?family=Inconsolata:400,700"
  -- Telescope logo font
  elAttr "link" ("href" =: "http://fonts.googleapis.com/css?family=Coustard:900" <> "rel" =: "stylesheet" <> "type" =: "text/css") $ return ()
  stylesheet "//maxcdn.bootstrapcdn.com/font-awesome/4.3.0/css/font-awesome.min.css"
  stylesheet "//cdnjs.cloudflare.com/ajax/libs/skeleton/2.0.4/skeleton.min.css"
  stylesheet "css/hscolour-default.css"
  stylesheet "css/css.css"
  -- <link rel="shortcut icon" href="favicon.png" />
  -- <link rel="apple-touch-icon" href="apple-touch-icon.png" /> 

ahref u t = elAttr "a" ("href" =: u) $ text t

body rootURL = do
  fallback $ el "p" $ do
    text "Sorry, your browser is not supported. A simplified version of the presentation follows. To get the full experience, please use a recent version of Chrome, Firefox, or Safari, or contact "
    ahref "mailto:info@obsidian.systems" "info@obsidian.systems"
  ryanFooter
  impressDiv $ slides rootURL

--TODO: should we have doubleInput, too?
--TODO: move to reflex-dom
--TODO: Disallow bad inputs
integerInput :: MonadWidget t m => m (Dynamic t Integer)
integerInput = do
  x <- textInput
  mapDyn (fromMaybe 0 . readMay) $ _textInput_value x

--TODO: Tab key should only move between controls on the *current* slide

buttonWithIcon :: MonadWidget t m => String -> String -> m (Event t ())
buttonWithIcon i t = do
  (e, _) <- el' "button" $ do
    icon i
    text $ " " <> t
  return $ _el_clicked e

slideWidth :: Int
slideWidth = 2500

slideHeight :: Int
slideHeight = 1500

slides :: forall t m. MonadWidget t m => String -> m ()
slides rootURL = do
  -- Part 1
  introSlides def
  twitterSlides rootURL $ def & y +~ slideHeight * 1
  reflexDemoSlides $ def & y +~ slideHeight * 2
  -- Break
  breakSlide $ def & y +~ slideHeight * 3
  -- Part 2
  frpRequirementsSlides $ def & y +~ slideHeight * 4
  reflexSemanticsSlides $ def & y +~ slideHeight * 5
  nextStepsSlides $ def & y +~ slideHeight * 6

introSlides :: forall t m. MonadWidget t m => SlideConfig -> m ()
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

JS(htmlElementCreateShadowRoot_, "$1.createShadowRoot()", JSRef HTMLElement -> IO HTMLElement)

htmlElementCreateShadowRoot = htmlElementCreateShadowRoot_ . unHTMLElement

reflexDemoSlides :: forall t m. MonadWidget t m => SlideConfig -> m ()
reflexDemoSlides cfg = do
  slide Nothing "" (cfg & x +~ slideWidth * 0) $ do
    el "h1" $ text "Made with Reflex"
  slide Nothing "" (cfg & x +~ slideWidth * 1) $ do
    el "h1" $ text "reflex-todomvc"
    elAttr "div" ("style" =: "width:1920px;height:1200px;") $ do
      e <- buildEmptyElement "div" ("class" =: "hidden-scroll" <> "style" =: "width:960px;height:600px;transform-origin: 0 0 0;transform:scale(2,2);overflow:auto" :: Map String String)
      eShadowRoot <- liftIO $ htmlElementCreateShadowRoot e
      subWidget (toNode eShadowRoot) $ do
        el "head" $ do
          el "style" $ text "@import \"todomvc/css.css\""
        el "body" $ do
          todoMVC
  slide Nothing "" (cfg & x +~ slideWidth * 2) $ do
    el "h1" $ text "Redline" --TODO: Logo
  slide Nothing "" (cfg & x +~ slideWidth * 3) $ do
    elAttr "h1" ("style" =: "font-family:'Coustard',serif;font-weight:900") $ text "Telescope"
  slide Nothing "" (cfg & x +~ slideWidth * 4
                        & y -~ (slideHeight `div` 5)
                        & scale *~ 11
                   ) $ do
    el "h1" $ text "This presentation"
    el "h4" $ text "Built with Reflex.Dom, impress.js, and Snap"
    elAttr "div" ("style" =: "width:1920px;height:1050px;") $ return ()

breakSlide :: forall t m. MonadWidget t m => SlideConfig -> m ()
breakSlide cfg = slide Nothing "" cfg $ do
  el "h1" $ text "Try it out!"
  elAttr "pre" ("style" =: "font-size:larger") $ text $ trimLeading [r|
      git clone https://github.com/ryantrinkle/try-reflex
      cd try-reflex
      ./try-reflex
    |]

frpRequirementsSlides :: forall t m. MonadWidget t m => SlideConfig -> m ()
frpRequirementsSlides cfg = do
  slide Nothing "" (cfg & x +~ slideWidth * 0) $ do
    el "h3" $ text "Practical systems must be expressive, comprehensible, and efficient"
  slide Nothing "" (cfg & x +~ slideWidth * 1) $ do
    el "h3" $ text "Practical FRP should support dynamic data flow"
  slide Nothing "" (cfg & x +~ slideWidth * 2) $ do
    el "h3" $ text "Practical FRP should support a variety of concepts of time"
  slide Nothing "" (cfg & x +~ slideWidth * 3) $ do
    el "h3" $ text "Practical FRP should be fully deterministic"
  slide Nothing "" (cfg & x +~ slideWidth * 4) $ do
    el "h3" $ text "Practical FRP should use idiomatic Haskell"
  slide Nothing "" (cfg & x +~ slideWidth * 5) $ do
    el "h3" $ text "Practical FRP should have good performance"
  slide Nothing "" (cfg & x +~ slideWidth * 6) $ do
    el "h3" $ text "Practical FRP should be fully garbage-collectable"

reflexSemanticsSlides :: forall t m. MonadWidget t m => SlideConfig -> m ()
reflexSemanticsSlides cfg = do
  slide Nothing "" (cfg & x +~ slideWidth * 0) $ do
    el "h1" $ text "Types"
    examplePre [r|
      Event t a
      
      Behavior t a
    |] mempty
  slide Nothing "" (cfg & x +~ slideWidth * 1) $ do
    el "h1" $ text "Degenerate"
    examplePre [r|
      never :: Event t a

      constant :: a -> Behavior t a
    |] mempty
  slide Nothing "" (cfg & x +~ slideWidth * 2) $ do
    el "h1" $ text "Mapping"
    examplePre [r|
      push :: (a -> PushM t (Maybe b)) -> Event t a -> Event t b

      pull :: PullM t a -> Behavior t a
    |] mempty
  slide Nothing "" (cfg & x +~ slideWidth * 3) $ do
    el "h1" $ text "Bulk"
    examplePre [r|
      merge :: GCompare k => DMap (WrapArg (Event t) k) -> Event t (DMap k)

      fan :: GCompare k => Event t (DMap k) -> EventSelector t k
      select :: EventSelector t k -> k a -> Event t a
    |] mempty
  slide Nothing "" (cfg & x +~ slideWidth * 4) $ do
    el "h1" $ text "Higher-order"
    examplePre [r|
      switch :: Behavior t (Event t a) -> Event t a

      coincidence :: Event t (Event t a) -> Event t a
    |] mempty
  slide Nothing "" (cfg & x +~ slideWidth * 5) $ do
    el "h1" $ text "Monadic"
    examplePre [r|
      sample :: MonadSample t m => Behavior t a -> m a
      instance MonadSample t (PullM t)
      instance MonadSample t (PushM t)

      hold :: MonadHold t m => a -> Event t a -> m (Behavior t a)
      instance MonadHold t (PushM t)
    |] mempty

nextStepsSlides :: forall t m. MonadWidget t m => SlideConfig -> m ()
nextStepsSlides cfg = do
  slide Nothing "" (cfg & x +~ slideWidth * 0) $ do
    el "h2" $ text "The Future of Reflex"
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
    

reflexTypes = Map.fromList [ ("constDyn", "a -> Dynamic t a")
                           , ("tag", "Behavior t a -> Event t b -> Event t a")
                           , ("current", "Dynamic t a -> Behavior t a")
                           , ("dynText", "Dynamic t String -> m ()")
                           , ("holdDyn", "a -> Event t a -> Dynamic t a")
                           , ("textArea", "TextAreaConfig t -> m (TextArea t)")
                           , ("mapDyn", "(a -> b) -> Dynamic t a -> m (Dynamic t b)")
                           , ("display", "(Show a) => Dynamic t a -> m ()")
                           , ("text", "String -> m ()")
                           , ("fmap", "(Functor f) => (a -> b) -> f a -> f b")
                           , ("el", "MonadWidget t m => String -> m a -> m a")
                           , ("ffilter", "(FunctorMaybe f) => (a -> Bool) -> f a -> f a")
                           , ("foldDyn", "(a -> b -> b) -> b -> Event t a -> m (Dynamic t b)")
                           ]

-- union is biased toward reflexTypes 
withReflexTypes = Map.union reflexTypes

twitterSlides :: forall t m. MonadWidget t m => String -> SlideConfig -> m ()
twitterSlides rootURL cfg = do
  let twitterSlideTypes = withReflexTypes $ Map.fromList
        [ ("newTweet", "Event t String")
        , ("tweetBox", "TextArea t")
        , ("tweetButton", "Event t ()")
        , ("latestTweet", "Dynamic t String")
        , ("tweetHistory", "Dynamic t [String]")
        ]
  slide Nothing "" (cfg & x +~ slideWidth * 0) $ do
    el "h1" $ text "#reflexFRP"
    el "h2" $ text "Building a Twitter frontend"
  slide Nothing "" (cfg & x +~ slideWidth * 1) $ do
    el "h4" $ text "An input and its value"
    examplePre [r|
       do tweetBox <- textArea def
          dynText $ value tweetBox
      |] twitterSlideTypes
    do tweetBox <- textArea def
       dynText $ value tweetBox
    return ()
  slide Nothing "" (cfg & x +~ slideWidth * 2) $ do
    el "h4" $ text "Adding an attribute"
    examplePre [r|
       do tweetBox <- textArea $
            def & attributes .~ constDyn ("maxlength" =: "140")
          dynText $ value tweetBox
      |] twitterSlideTypes
    do tweetBox <- textArea $
         def & attributes .~ constDyn ("maxlength" =: "140")
       dynText $ value tweetBox
    return ()
  slide Nothing "" (cfg & x +~ slideWidth * 3) $ do
    el "h4" $ text "Displaying the length of the input"
    examplePre [r|
       do tweetBox <- textArea $
            def & attributes .~ constDyn ("maxlength" =: "140")
          el "div" $ do
            numChars <- mapDyn length $ value tweetBox
            display numChars
            text " characters"
      |] twitterSlideTypes
    do tweetBox <- textArea $
         def & attributes .~ constDyn ("maxlength" =: "140")
       el "div" $ do
         numChars <- mapDyn length $ value tweetBox
         display numChars
         text " characters"
    return ()
  slide Nothing "" (cfg & x +~ slideWidth * 4) $ do
    el "h4" $ text "Showing a value when an event occurs"
    examplePre [r|
      do newTweet <- el "div" $ do
           tweetBox <- textArea $
             def & attributes .~ constDyn ("maxlength" =: "140")
           tweetButton <- buttonWithIcon "twitter" "Tweet!"
           displayNumChars tweetBox
           return $ tag (current (value tweetBox)) tweetButton
         el "div" $ do
           latestTweet <- holdDyn "" newTweet
           text "Last status: "
           dynText latestTweet
    |] twitterSlideTypes
    do newTweet <- el "div" $ do
         tweetBox <- textArea $
           def & attributes .~ constDyn ("maxlength" =: "140")
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
           rec tweetBox <- textArea $
                 def & attributes .~ constDyn ("maxlength" =: "140")
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
         rec tweetBox <- textArea $
               def & attributes .~ constDyn ("maxlength" =: "140")
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
           rec tweetBox <- textArea $
                 def & attributes .~ constDyn ("maxlength" =: "140")
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
         rec tweetBox <- textArea $
               def & attributes .~ constDyn ("maxlength" =: "140")
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
           rec tweetBox <- textArea $
                 def & attributes .~ constDyn ("maxlength" =: "140")
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
         rec tweetBox <- textArea $
               def & attributes .~ constDyn ("maxlength" =: "140")
                   & setValue .~ fmap (\_ -> "") tweetButton
             tweetButton <- buttonWithIcon "twitter" "Tweet!"
         displayNumChars tweetBox
         return $ ffilter (/="") $ tag (current (value tweetBox)) tweetButton
       el "div" $ do
         tweetHistory <- foldDyn (:) [] newTweet
         text "Tweet history: "
         display tweetHistory
  slide Nothing "" (cfg & x +~ slideWidth * 9) $ do
    c <- el "div" $ twitterAuthorize rootURL
    disableUntilAuth <- mapDyn (\x -> if x == Nothing then ("disabled" =: "true" <> "style" =: "cursor:not-allowed;") else mempty) c
    twote <- el "div" $ do
      rec t <- input' "text" "" (fmap (const "") twote) (constDyn mempty)
          send <- liftM (_el_clicked . fst) $ elDynAttr' "button" disableUntilAuth $ text "Tweet"
          tweet :: Event t (Maybe Credential, String) <- liftM (flip tagDyn send) $ combineDyn (,) c (_textInput_value t)
          twote <- performRequestAsync $ fmap (\(x,y) -> toTweetReq x y) $  fmapMaybe id $ fmap biseqFirst tweet
      return twote
    el "div" $ do
      s :: Event t StreamingAPI <- startStream $ fmapMaybe id $ updated c
      tweetList =<< mapDyn (Map.fromList . zip [(1::Int)..]) =<< foldDyn (:) [] (fmapMaybe (\x -> case x of
                                                                                                  SStatus a -> Just a
                                                                                                  _ -> Nothing) s)
  return ()

tweetControl :: MonadWidget t m => m (Event t String)
tweetControl = el "div" $ do
  rec tweetBox <- textArea $
        def & attributes .~ constDyn ("maxlength" =: "140")
            & setValue .~ fmap (const "") tweetButton
      tweetButton <- buttonWithIcon "twitter" "Tweet!"
  displayNumChars tweetBox
  return $ ffilter (/="") $ tag (current (value tweetBox)) tweetButton

type Credential = SimpleQuery

twitterAuthorize :: forall t m. MonadWidget t m => String -> m (Dynamic t (Maybe Credential))
twitterAuthorize rootURL = do
  r <- performRequestAsync . fmap (const $ XhrRequest "GET" ("/oauth?callback=" <> rootURL <> "/blank") def) =<< getPostBuild
  url <- holdDyn "" $ fmapMaybe id $ fmap respBody r
  auth <- buttonWithIcon "key" "Authorize"
  win <- performEvent (fmap (\u -> liftIO $ windowOpen u "test" "height=250, width=250") $ tagDyn url auth)
  temp <- performEventAsync (fmap (\w cb -> liftIO $ waitForOauth w cb) win)
  cr <- performRequestAsync $ fmap (\tc -> XhrRequest "GET" ("/twitter/secret" <> toQueryString tc) def) temp
  let c :: Event t Credential = fmapMaybe readMay $ fmapMaybe respBody cr
  creds <- holdDyn Nothing $ fmap Just c
  let loading = fmap (const $ icon "spinner fa-pulse") auth
      check = fmap (const $ icon "check") (updated creds)
  dyn =<< holdDyn blank (leftmost [loading, check])
  return creds

displayNumChars :: MonadWidget t m => TextArea t -> m ()
displayNumChars tweetBox = el "div" $ do
  numChars <- mapDyn length $ value tweetBox
  display numChars
  text " characters"

timeline :: forall t m a. MonadWidget t m => Dynamic t (Maybe Credential) -> Event t a -> Dynamic t (Map String String) -> m ()
timeline c update active = el "div" $ do
  getTL <- liftM (_el_clicked . fst) $ elDynAttr' "button" active $ text "Get My Timeline"
  tlr <- performRequestAsync (fmap (\cred -> XhrRequest "GET" ("/twitter/timeline" <> toQueryString cred) def) $ fmapMaybe id $ tagDyn c $ leftmost [getTL, fmap (const ()) update])
  dyn =<< holdDyn blank (leftmost [fmap (const $ icon "spinner fa-pulse") getTL, fmap (const blank) tlr])
  let tl :: Event t [Status] = fmapMaybe (join . fmap (decode . LBS.fromStrict . encodeUtf8 . T.pack) . respBody) tlr
  statuses <- holdDyn Map.empty $ fmap (Map.fromList . take 5 . zip [(1::Int)..]) tl
  tweetList statuses
  return ()

tweetList :: (MonadWidget t m, Ord k) => Dynamic t (Map k Status) -> m (Dynamic t (Map k ()))
tweetList statuses = elAttr "div" ("style" =: "") $ elClass "ul" "fa-ul" $ list statuses $ \s -> do
  el "li" $ do
    elClass "i" "fa-li fa fa-twitter" $ return ()
    el "strong" $ dynText =<< mapDyn (T.unpack . userName . statusUser) s
    el "p" $ dynText =<< mapDyn (T.unpack . statusText) s
    elAttr "p" ("style" =: "font-size:50%;") $ dynText =<< mapDyn (show . statusCreatedAt) s

startStream :: forall t m. MonadWidget t m => Event t Credential -> m (Event t StreamingAPI)
startStream cred = liftM (switch . current) $ widgetHold (return never) (fmap stream cred)

stream :: MonadWidget t m => Credential -> m (Event t StreamingAPI)
stream c = do
  ws <- webSocket $ "/twitter/userStream" <> toQueryString c
  return $ fmapMaybe id $ fmap (decode . LBS.fromStrict) ws

icon i = elClass "i" ("fa fa-" <> i) $ return ()

biseqFirst :: Monad m => (m a, b) -> m (a, b)
biseqFirst (a,b) = bisequence (a, return b)

toTweetReq :: Credential -> String -> XhrRequest
toTweetReq c s = XhrRequest "POST" "/twitter/status" $ def { _xhrRequestConfig_sendData = Just $ show (c,s) }

respBody :: XhrResponse -> Maybe String
respBody = fmap fromJSString . _xhrResponse_body

toQueryString :: Credential -> String
toQueryString = T.unpack . decodeUtf8 . renderSimpleQuery True

waitForOauth :: Window -> (Credential -> IO ()) -> IO ()
waitForOauth w cb = void $ forkIO $ do
  let go = do
        l <- windowLocationHref w
        case l of
             Just l' | l' /= "about:blank"
                     , Just uri <- parseURI l'
                     -> do
                          windowClose w
                          cb $ parseSimpleQuery $ encodeUtf8 $ T.pack $ uriQuery uri
             _ -> do
               threadDelay 250000
               go
  go

lightenPunctuation :: MonadWidget t m => String -> m ()
lightenPunctuation s = do
  let (a,b) = Prelude.span Data.Char.isAlphaNum s
  el "span" $ text a
  case b of
       [] -> return ()
       (x:xs) -> do
         elAttr "span" ("class" =: "light") $ text [x]
         lightenPunctuation xs

logo :: MonadWidget t m => String -> m ()
logo k = elAttr "span" ("class" =: ("logo " <> k)) $ withPunct "OBSIDIAN" "." "SYSTEMS" 

withPunct :: MonadWidget t m => String -> String -> String -> m ()
withPunct a p b = do
  text $ (maybe "" id) $ initMay a
  elAttr "span" ("class" =: "") $ text $ (maybe "" (:[])) $ lastMay a
  elAttr "span" ("class" =: "punctuation") $ text p
  text b

ryanFooter = elAttr "a" ("class" =: "logo logo-bottom" <> "href" =: "mailto:ryan.trinkle@obsidian.systems")  $ do
  withPunct "RYAN" "." "TRINKLE"
  withPunct "" "@" ""
  withPunct "OBSIDIAN" "." "SYSTEMS"
