module Test (main) where

import Test.Tasty

import qualified Test.Backend as Backend

main :: IO ()
main = defaultMain $ testGroup "Backend" Backend.tests
