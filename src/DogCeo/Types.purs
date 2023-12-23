module DogCeo.Types
  ( Breed
  , BreedGroup
  , Page(..)
  ) where

import Data.Maybe (Maybe)

data Page
  = BreedsPage
  | ImagesPage { breed :: Breed }

type BreedGroup =
  { name :: String
  , subBreeds :: Array String
  }

type Breed =
  { name :: String
  , subBreed :: Maybe String
  }
