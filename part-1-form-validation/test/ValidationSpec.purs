module Test.ValidationSpec where

import Prelude
import Validation

import Test.QuickCheck (Result(..), (/==), (===))
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (shouldEqual, shouldNotSatisfy, shouldSatisfy)
import Test.Spec.QuickCheck (quickCheck, quickCheckPure)

spec :: Spec Unit
spec = do
  describe "validation" $ do
    it "hello world" $ do
      hello "world" `shouldEqual` "Hello, world"

    pending "todo example"

