module DogCeo.Types
  ( Breed
  , BreedGroup
  , Page(..)
  , ImageMap
  , ApiResult(..)
  ) where

import Prelude

import Data.Map (Map)
import Data.Maybe (Maybe)

data ApiResult a
  = Loading
  | Success a

data Page
  = BreedsPage
  | ImagesPage { breed :: String }

type BreedGroup =
  { name :: String
  , subBreeds :: Array String
  }

type Breed =
  { name :: String
  , subBreed :: Maybe String
  }

type ImageMap = Map String (Array String)
