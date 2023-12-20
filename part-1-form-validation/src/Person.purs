module Person where

import Prelude

import Data.Either (Either(..))

data MaritalStatus = Married | Divorced | Single
newtype Name = Name String
newtype SocialSecurity = SocialSecurity String
newtype PhoneNumber = PhoneNumber String

type Person f =
  { firstName :: f Name
  , lastName :: f Name
  , socialSecurity :: f SocialSecurity
  , maritalStatus :: f MaritalStatus
  , phoneNumber :: f PhoneNumber
  }

type Validator a = String -> Either String a

personValidator :: Person Validator
personValidator =
  -- todo: non empty
  { firstName: Name >>> Right
  , lastName: Name >>> Right
  -- todo: every char is a digit and the length is 9
  , socialSecurity: SocialSecurity >>> Right
  -- todo: parse string to enum
  , maritalStatus: const $ Right Single
  -- todo: every char is a digit and the length is 10 (optional country code?)
  , phoneNumber: PhoneNumber >>> Right
  }

