module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpec)
import Test.ValidationSpec as ValidationSpec

main :: Effect Unit
main = launchAff_
  $ runSpec [ consoleReporter ]
  $ do
      ValidationSpec.spec
