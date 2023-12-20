module DogBreed.Types
  ( Breed
  , Page(..)
  , ImageMap
  ) where

import Prelude

import Data.Map (Map)

data Page
  = BreedListPage
  | BreedDetailsPage String

type Breed =
  { name :: String
  , subBreeds :: Array String
  }

type ImageMap = Map String (Array String)

