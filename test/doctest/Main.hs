module Main (main) where

import System.IO (IO)

import qualified Test.DocTest

main :: IO ()
main = Test.DocTest.doctest
  [ "src"
  , "-XFlexibleInstances"
  , "-XLambdaCase"
  , "-XNoImplicitPrelude"
  , "-XNoMonomorphismRestriction"
  , "-XOverloadedStrings"
  , "-XQuasiQuotes"
  , "-XScopedTypeVariables"
  , "-XTemplateHaskell"
  , "-XTupleSections"
  ]
