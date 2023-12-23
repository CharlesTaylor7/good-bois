module DogCeo.Component.App
  ( component
  ) where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import DogCeo.Api.Breeds as BreedsApi
import DogCeo.Api.Images as ImagesApi
import DogCeo.Component.Breeds as BreedsPage
import DogCeo.Component.Images as ImagesPage
import DogCeo.Types (ApiResult(..), Breed, BreedGroup, Page(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))

type Slots =
  ( breedsPage :: BreedsPage.Slot
  , imagesPage :: ImagesPage.Slot
  )

_breedsPage = Proxy :: Proxy "breedsPage"
_imagesPage = Proxy :: Proxy "imagesPage"

type State =
  { breeds :: ApiResult (Array BreedGroup)
  , imagesCache :: Map Breed (Array String)
  , page :: Page
  }

data Action
  = Init
  | HandleBreedsPage BreedsPage.Output
  | HandleImagesPage ImagesPage.Output

component :: forall query input output monad. MonadAff monad => H.Component query input output monad
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Init
        }
    }

initialState :: forall input. input -> State
initialState _ =
  { breeds: Loading
  , imagesCache: Map.empty
  , page: BreedsPage
  }

render :: forall monad. MonadAff monad => State -> H.ComponentHTML Action Slots monad
render state =
  case state.page of
    BreedsPage ->
      HH.slot _breedsPage unit BreedsPage.component
        { breeds: state.breeds
        }
        HandleBreedsPage

    ImagesPage { breed } ->
      HH.slot _imagesPage unit ImagesPage.component
        { breed
        , images:
            state.imagesCache
              # Map.lookup breed
              # maybe Loading Success
        }
        HandleImagesPage

handleAction :: forall output monad. MonadAff monad => Action -> H.HalogenM State Action Slots output monad Unit
handleAction = case _ of
  Init -> do
    void $ H.fork $ do
      breeds <- BreedsApi.fetch
      H.modify_ \state -> state
        { breeds = Success breeds
        }

  -- Fetches the images if we haven't selected this breed before
  -- Navigate to the images page for this breed
  HandleBreedsPage (BreedsPage.Selected breed) -> do
    { imagesCache } <- H.get
    when (Map.lookup breed imagesCache == Nothing) $ void $ H.fork $ do
      images <- ImagesApi.fetch breed
      H.modify_ \state -> state
        { imagesCache = state.imagesCache # Map.insert breed images
        }

    H.modify_ \state -> state
      { page = ImagesPage { breed } }

  HandleImagesPage ImagesPage.BackToBreeds ->
    H.modify_ \state -> state { page = BreedsPage }
