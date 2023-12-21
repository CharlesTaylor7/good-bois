module DogBreed.Types
  ( Breed
  , Page(..)
  , ImageMap
  , ApiResult(..)
  ) where

import Prelude

import Data.Map (Map)

data ApiResult a
  = Loading
  | Success a

data Page
  = BreedListPage
  | BreedDetailsPage String

type Breed =
  { name :: String
  , subBreeds :: Array String
  }

type ImageMap = Map String (Array String)

