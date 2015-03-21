{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, QuasiQuotes, JavaScriptFFI, CPP, ForeignFunctionInterface #-}
import Prelude hiding (head)

import ReflexTalk.Example

import Reflex
import Reflex.Dom
import Reflex.ImpressJs
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
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
  stylesheet "http://fonts.googleapis.com/css?family=Alegreya"
  stylesheet "http://fonts.googleapis.com/css?family=Josefin+Sans:300,400"
  stylesheet "http://fonts.googleapis.com/css?family=Karma:400,300"
  stylesheet "css/css.css"
  -- <link rel="shortcut icon" href="favicon.png" />
  -- <link rel="apple-touch-icon" href="apple-touch-icon.png" /> 

ahref u t = elAttr "a" ("href" =: u) $ text t

body origin = do
  fallback $ el "p" $ do
    text "Sorry, your browser is not supported. A simplified version of the presentation follows, but to get the full experience, please use a recent version of Chrome, Firefox, or Safari, or contact "
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

slides :: forall t m. MonadWidget t m => String -> [m ()]
slides origin =
  [ slide Nothing "slide" (def { _x = 0 }) $ el "q" $ do
       el "h1" $ text "Reflex:"
       el "h2" $ text "Practical Functional Reactive Programming"
       el "h3" $ text "Ryan Trinkle"
       el "h4" $ text "Obsidian.Systems" --TODO: Use the words reflex and obsidian more
  , slide Nothing "slide" (def { _x = 1000 }) $ do
       $(example [r|
          do clicked <- button "Click me!"
             numClicks :: Dynamic t Integer <- count clicked
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
  , twitter origin
  ]

twitter :: forall t m. MonadWidget t m => String -> m ()
twitter origin = slide Nothing "slide" (def {_x = 3000 }) $ do
  r <- performRequestAsync . fmap (const $ XhrRequest "GET" ("/oauth?callback=" <> origin <> "/blank") def) =<< getPostBuild
  url <- holdDyn "" $ fmapMaybe id $ fmap respBody r
  l <- link "open"
  win <- performEvent (fmap (\u -> liftIO $ windowOpen u "test" "height=250, width=250") $ tagDyn url (_link_clicked l))
  temp <- performEventAsync (fmap (\w cb -> liftIO $ waitForOauth w cb) win)
  cr <- performRequestAsync $ fmap (\tc -> XhrRequest "GET" ("/twitter/secret" <> toQueryString tc) def) temp
  let c :: Event t SimpleQuery = fmapMaybe readMay $ fmapMaybe respBody cr
  timeline <- performRequestAsync $ fmap (\cred -> XhrRequest "GET" ("twitter/timeline" <> toQueryString cred) def) c
  display =<< holdDyn Nothing (fmap respBody timeline)
  return ()

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
