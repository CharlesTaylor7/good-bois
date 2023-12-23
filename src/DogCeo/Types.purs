module DogCeo.Types
  ( Breed
  , BreedGroup
  , ApiResult(..)
  ) where

import Data.Maybe (Maybe)

data ApiResult a
  = Loading
  | Success a

type BreedGroup =
  { name :: String
  , subBreeds :: Array String
  }

type Breed =
  { name :: String
  , subBreed :: Maybe String
  }
