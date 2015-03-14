import Prelude hiding (head)
import Reflex
import Reflex.Dom
import Reflex.ImpressJs
import Control.Monad
import Data.Default
import Data.Monoid
import qualified Data.Map as Map
import Data.Map (Map)

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

body = do
  fallback $ el "p" $ do
    text "Sorry, your browser is not supported. A simplified version of the presentation follows, but to get the full experience, please use a recent version of Chrome, Firefox, or Safari, or contact "
    ahref "mailto:info@obsidian.systems" "info@obsidian.systems"
  impressDiv [ slide1, slide2 ] 

ahref u t = elAttr "a" ("href" =: u) $ text t

slide1 = slide Nothing "slide" (def { _x = -1000, _y = -1500 }) $ do
  el "h1" $ text "Example"
  el "h2" $ text "This is an example"
  el "h3" $ text "Obsidian Systems"
slide2 = slide Nothing "slide" (def { _x = 0, _y = -1500 }) $ el "q" $ text "This is a test"

