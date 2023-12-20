module Test.ValidationSpec where

import Prelude

import Test.QuickCheck (Result(..), (/==), (===))
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (shouldEqual, shouldNotSatisfy, shouldSatisfy)
import Test.Spec.QuickCheck (quickCheck, quickCheckPure)

spec :: Spec Unit
spec = do
  describe "validation" $ do
    pending "todo example"

