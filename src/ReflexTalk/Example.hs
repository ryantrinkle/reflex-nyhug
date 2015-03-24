{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module ReflexTalk.Example where

import Language.Haskell.TH.Syntax
import Language.Haskell.Meta.Parse
import Language.Haskell.HsColour.CSS
import Language.Haskell.HsColour.Colourise
import Data.Char

import Reflex
import Reflex.Dom

strExp :: String -> Q Exp
strExp = either fail return . parseExp

trimLeading :: String -> String
trimLeading s = unlines $ map (drop numToTrim) ls
  where ls = reverse $ dropWhile isAllWhitespace $ reverse $ dropWhile isAllWhitespace $ lines s
        isAllWhitespace x = length (filter (not . isSpace) x) == 0
        numToTrim = minimum $ map (length . takeWhile (==' ')) ls

example :: String -> Q Exp
example s = [| do
    examplePre s
    $(strExp s)
  |]

examplePre :: MonadWidget t m => String -> m ()
examplePre s = 
  divClass "example" $ do
    elDynHtmlAttr' "div" ("class" =: "code") $ constDyn coloured
    el "hr" $ return ()
  where coloured = hscolour True (trimLeading s)
