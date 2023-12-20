module Validation1
  ( FormData
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

data FormData a
  = RawData String
  | ErrorData (Maybe String)
  | ValidData a

type Validator a = a -> Either String a

