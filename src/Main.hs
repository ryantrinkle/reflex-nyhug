{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, QuasiQuotes, JavaScriptFFI, CPP, ForeignFunctionInterface, RecursiveDo #-}
import Prelude hiding (head)

import ReflexTalk.Example

import Reflex
import Reflex.Dom
import Reflex.ImpressJs
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
  origin <- globalLocationOrigin
  mainWidgetWithHead head $ body origin
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
  stylesheet "//fonts.googleapis.com/css?family=Josefin+Sans:300,400"
  stylesheet "//fonts.googleapis.com/css?family=Karma:400,300"
  stylesheet "//maxcdn.bootstrapcdn.com/font-awesome/4.3.0/css/font-awesome.min.css"
  stylesheet "css/css.css"
  -- <link rel="shortcut icon" href="favicon.png" />
  -- <link rel="apple-touch-icon" href="apple-touch-icon.png" /> 

ahref u t = elAttr "a" ("href" =: u) $ text t

body origin = do
  fallback $ el "p" $ do
    text "Sorry, your browser is not supported. A simplified version of the presentation follows. To get the full experience, please use a recent version of Chrome, Firefox, or Safari, or contact "
    ahref "mailto:info@obsidian.systems" "info@obsidian.systems"
  impressDiv $ slides origin

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

slides :: forall t m. MonadWidget t m => String -> m ()
slides origin = do
  slide Nothing "slide" (def { _x = 0 }) $ el "q" $ do
     el "h1" $ text "Reflex:"
     el "h2" $ text "Practical Functional Reactive Programming"
     el "h3" $ text "Ryan Trinkle"
     el "h4" $ text "Obsidian.Systems" --TODO: Use the words reflex and obsidian more
  slide Nothing "slide" (def { _x = 1000 }) $ do
     $(example [r|
        do tweetBox <- textArea def
           display $ value tweetBox
        |])
     return ()
  slide Nothing "slide" (def { _x = 2000 }) $ do
     $(example [r|
        do tweetBox <- el "div" $ textArea $
             def & attributes .~ constDyn ("maxlength" =: "140")
           el "div" $ do
             numChars <- mapDyn length $ value tweetBox
             display numChars
             text " characters"
        |])
     return ()
  slide Nothing "slide" (def { _x = 3000 }) $ do
     do newTweet <- el "div" $ do
          rec tweetBox <- textArea $
                def & attributes .~ constDyn ("maxlength" =: "140")
                    & textAreaConfig_setValue .~ fmap (const "") tweetButton
              tweetButton <- buttonWithIcon "twitter" "Tweet!"
          el "div" $ do
            numChars <- mapDyn length $ value tweetBox
            display numChars
            text " characters"
          return $ tag (current (value tweetBox)) tweetButton
        el "div" $ do
          latestStatus <- foldDyn (:) [] newTweet
          display latestStatus
     return ()
  twitter origin

twitter :: forall t m. MonadWidget t m => String -> m ()
twitter origin = slide Nothing "slide" (def {_x = 4000 }) $ do
  r <- performRequestAsync . fmap (const $ XhrRequest "GET" ("/oauth?callback=" <> origin <> "/blank") def) =<< getPostBuild
  url <- holdDyn "" $ fmapMaybe id $ fmap respBody r
  c <- el "div" $ do
    auth <- button "Authorize"
    win <- performEvent (fmap (\u -> liftIO $ windowOpen u "test" "height=250, width=250") $ tagDyn url auth)
    temp <- performEventAsync (fmap (\w cb -> liftIO $ waitForOauth w cb) win)
    cr <- performRequestAsync $ fmap (\tc -> XhrRequest "GET" ("/twitter/secret" <> toQueryString tc) def) temp
    let c :: Event t SimpleQuery = fmapMaybe readMay $ fmapMaybe respBody cr
    creds <- holdDyn Nothing $ fmap Just c
    dyn =<< mapDyn (maybe blank $ const (icon "check")) creds
    return creds
  disableUntilAuth <- mapDyn (\x -> if x == Nothing then ("disabled" =: "true" <> "style" =: "cursor:not-allowed;") else mempty) c
  timeline <- el "div" $ do
    getTL <- liftM (_el_clicked . fst) $ elDynAttr' "button" disableUntilAuth $ text "Get My Timeline"
    tlr <- performRequestAsync (fmap (\cred -> XhrRequest "GET" ("twitter/timeline" <> toQueryString cred) def) $ fmapMaybe id $ tagDyn c getTL)
    let tl :: Event t [Status] = fmapMaybe (join . fmap (decode . LBS.fromStrict . encodeUtf8 . T.pack) . respBody) tlr
    statuses <- holdDyn Map.empty $ fmap (Map.fromList . zip [(1::Int)..]) tl
    elClass "ul" "fa-ul" $ list statuses $ \s -> do
      el "li" $ do
        elClass "i" "fa-li fa fa-twitter" $ return ()
        el "strong" $ dynText =<< mapDyn (T.unpack . userName . statusUser) s
        el "p" $ dynText =<< mapDyn (T.unpack . statusText) s
        elAttr "p" ("style" =: "font-size:50%;") $ dynText =<< mapDyn (show . statusCreatedAt) s
    return ()
  el "div" $ do
    rec t <- input' "text" "" (fmap (const "") twote) (constDyn mempty)
        send <- liftM (_el_clicked . fst) $ elDynAttr' "button" disableUntilAuth $ text "Tweet"
        tweet :: Event t (Maybe SimpleQuery, String) <- liftM (flip tagDyn send) $ combineDyn (,) c (_textInput_value t)
        twote <- performRequestAsync $ fmap (\(x,y) -> toTweetReq x y) $  fmapMaybe id $ fmap biseqFirst tweet

    return ()
    --dynText =<< holdDyn "" (fmap (maybe "" id . respBody) tl)
  return ()

icon i = elClass "i" ("fa fa-" <> i) $ return ()

biseqFirst :: Monad m => (m a, b) -> m (a, b)
biseqFirst (a,b) = bisequence (a, return b)

toTweetReq :: SimpleQuery -> String -> XhrRequest
toTweetReq c s = XhrRequest "POST" "/twitter/status" $ def { _xhrRequestConfig_sendData = Just $ show (c,s) }

respBody :: XhrResponse -> Maybe String
respBody = fmap fromJSString . _xhrResponse_body

toQueryString :: SimpleQuery -> String
toQueryString = T.unpack . decodeUtf8 . renderSimpleQuery True

waitForOauth :: Window -> (SimpleQuery -> IO ()) -> IO ()
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
