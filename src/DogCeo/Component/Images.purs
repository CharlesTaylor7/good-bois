module DogCeo.Component.Images
  ( component
  , Output(..)
  , Slot
  ) where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..))
import DogCeo.Api.Utils as Api
import DogCeo.Component.Images.Types (Action(..), ImageLoad(..), Input, State)
import DogCeo.Component.Images.View as View
import DogCeo.Routes (Route(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Router.Class (class MonadRouter)
import Halogen.Router.Class as HR
import Record as Record

data Output = FetchImages
type Slot id = forall query. H.Slot query Output id

component ::
  forall query monad.
  MonadRouter Route monad =>
  MonadAff monad =>
  H.Component query Input Output monad
component =
  H.mkComponent
    { initialState
    , render: View.render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Init
        , receive = Just <<< Receive
        }
    }

initialState :: Input -> State
initialState = Record.merge
  { page: 1
  , imageStatus: Map.empty
  }

handleAction ::
  forall monad.
  MonadRouter Route monad =>
  MonadAff monad =>
  Action ->
  H.HalogenM State Action () Output monad Unit
handleAction =
  case _ of
    Init -> do
      { images } <- H.get
      case images of
        Api.Success _ ->
          pure unit

        _ ->
          H.raise FetchImages

    Receive input ->
      H.modify_ $ Record.merge input

    NavBackToBreeds ->
      HR.navigate BreedsRoute

    GotoPreviousPage -> do
      { breed, page } <- H.get
      HR.navigate $ ImagesRoute { breed, page: page - 1 }

    GotoNextPage -> do
      { breed, page } <- H.get
      HR.navigate $ ImagesRoute { breed, page: page + 1 }

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
