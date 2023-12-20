module Validation
  ( Raw
  , Error
  , Valid
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

-- Functors:
-- Each functor corresponds to a stage of the form submission
-- (1) the user provides data for the form fields. No error / validation feedback is yet provided
-- (2) the user receives an error message for the form field
-- (3) the user receives confirmation the form field or entire form is valid, and submission goes through.

-- Many UIs collpase state 1 & 2 into the same step, which can lead to an annoying behavior where by the UI is validating while you type and showing annoying red lines while you type out your phone number.

-- isomorphic to Const String
newtype Raw (a :: Type) = Raw String

-- isomorphic to Const (Maybe String)
newtype Error (a :: Type) = Error (Maybe String)

-- isomorphic to Identity functor
newtype Valid (a :: Type) = Valid a

type Validator a = String -> Either String a
