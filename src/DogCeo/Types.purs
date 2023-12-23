module DogCeo.Types
  ( Breed
  , BreedGroup
  , Page(..)
  , ApiResult(..)
  ) where

import Data.Maybe (Maybe)

data ApiResult a
  = Loading
  | Success a

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
