{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module ReflexTalk.Example where

import Language.Haskell.TH.Syntax
import Language.Haskell.Meta.Parse
import Language.Haskell.HsColour.CSS
import Language.Haskell.HsColour.Colourise
import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Monoid

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
    examplePre s Map.empty
    $(strExp s)
  |]

example' :: String -> Map String String -> Q Exp
example' s ts = [| do
    examplePre s (Map.fromList ts')
    $(strExp s)
  |]
  where ts' = Map.toList ts


examplePre :: MonadWidget t m => String -> Map String String -> m ()
examplePre s types = 
  divClass "example" $ do
    elDynHtmlAttr' "div" ("class" =: "code") $ constDyn coloured
    el "hr" $ return ()
  where coloured' = hscolour True (trimLeading s)
        coloured = foldl' (\html (token, typ) -> replace (">" <> token <> "<") ("title=\"" <> token <> " :: " <> typ <> "\">" <> token <> "<") html) coloured' (Map.toList types)

replace old new = intercalate new . splitOn old
