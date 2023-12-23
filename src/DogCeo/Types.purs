module DogCeo.Types
  ( Breed
  , BreedGroup
  , ApiResult(..)
  ) where

import Data.Maybe (Maybe)
import Effect.Exception (Error)

data ApiResult a
  = Loading
  | Error Error
  | Success a

type BreedGroup =
  { name :: String
  , subBreeds :: Array String
  }

type Breed =
  { name :: String
  , subBreed :: Maybe String
  }
