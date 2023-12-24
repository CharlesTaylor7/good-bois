module DogCeo.Component.Images.Types where

import Prelude

import Data.Map (Map)
import DogCeo.Api.Utils as Api
import DogCeo.Types (Breed)

-- | The input is a subset of the state, so we describe its properties in one place as a row type
type InputRow =
  ( breed :: Breed
  , images :: Api.Result (Array String)
  )

type Input = Record InputRow

type State =
  { page :: Int
  , imageStatus :: Map String ImageLoad
  | InputRow
  }

data Action
  = NavBackToBreeds
  | Receive Input
  | GotoPreviousPage
  | GotoNextPage
  | ImageLoaded String
  | ImageNotFound String

data ImageLoad
  = LoadingImage
  | LoadedImage
  | ErrorImage

derive instance Eq ImageLoad

