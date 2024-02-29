module DogCeo.Component.Images.Types where

import DogCeo.Api.Utils as Api
import DogCeo.Component.Image as Image
import DogCeo.Types (Breed)

-- | The input is a subset of the state, so we describe its properties in one place as a row type
type InputRow =
  ( breed :: Breed
  , page :: Int
  , images :: Api.Result (Array String)
  )

type Input = Record InputRow

type State = Input

data Action
  = Init
  | Receive Input
  | NavToBreeds
  | NavToPreviousPage
  | NavToNextPage

type Slots =
  ( image :: Image.Slot Int
  )
