{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module ReflexTalk.Example where

import Language.Haskell.TH.Syntax
import Language.Haskell.Meta.Parse
import Language.Haskell.HsColour.InlineCSS
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
  elDynHtmlAttr' "div" ("style" =: "font-size:smaller;line-height:normal") $ constDyn coloured
  el "hr" $ return ()
  $(strExp s)
  |]
  where coloured = hscolour defaultColourPrefs False $ trimLeading s
