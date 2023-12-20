module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Gemini.ValidationSpec as ValidationSpec
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpec')

main :: Effect Unit
main = launchAff_
  $ runSpec' (defaultConfig { failFast = true }) [ consoleReporter ]
  $ do
      ValidationSpec.spec
