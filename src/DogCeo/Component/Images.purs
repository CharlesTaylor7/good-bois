module DogCeo.Component.Images
  ( component
  , Output(..)
  , Slot
  ) where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..))
import DogCeo.Component.Images.Types (Action(..), ImageLoad(..), Input, State)
import DogCeo.Component.Images.View as View
import Halogen as H
import Record as Record

type Slot id = forall query. H.Slot query Output id
data Output = BackToBreeds

component :: forall query monad. H.Component query Input Output monad
component =
  H.mkComponent
    { initialState
    , render: View.render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }

initialState :: Input -> State
initialState = Record.merge
  { page: 1
  , imageStatus: Map.empty
  }

handleAction :: forall monad. Action -> H.HalogenM State Action () Output monad Unit
handleAction =
  case _ of
    NavBackToBreeds ->
      H.raise BackToBreeds

    Receive input ->
      H.modify_ $ Record.merge input

    GotoPreviousPage ->
      H.modify_ $ \state -> state { page = state.page - 1 }

    GotoNextPage ->
      H.modify_ $ \state -> state { page = state.page + 1 }

    ImageNotFound src -> do
      H.modify_ \state -> state
        { imageStatus = state.imageStatus
            # Map.insert src ErrorImage
        }

    ImageLoaded src -> do
      H.modify_ \state -> state
        { imageStatus = state.imageStatus
            # Map.insert src LoadedImage
        }
