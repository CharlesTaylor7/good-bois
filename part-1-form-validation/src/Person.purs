module Person where

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

