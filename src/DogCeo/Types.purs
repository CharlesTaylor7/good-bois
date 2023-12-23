module DogCeo.Types
  ( Breed
  , BreedGroup
  ) where

import Data.Maybe (Maybe)

type BreedGroup =
  { name :: String
  , subBreeds :: Array String
  }

type Breed =
  { name :: String
  , subBreed :: Maybe String
  }
