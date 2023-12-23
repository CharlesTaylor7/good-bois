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
import DogCeo.Routes (Route(..))
import DogCeo.Types (ApiResult(..), Breed, BreedGroup)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Router.Class (class MonadRouter)
import Halogen.Router.Class as HR
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
  , route :: Maybe Route
  }

data Action
  = Init
  | ChangeRoute Route
  | HandleBreedsPage BreedsPage.Output
  | HandleImagesPage ImagesPage.Output

component ::
  forall query input output monad.
  MonadRouter Route monad =>
  MonadAff monad =>
  H.Component query input output monad
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
  , route: Just BreedsRoute
  }

render ::
  forall monad.
  MonadRouter Route monad =>
  MonadAff monad =>
  State ->
  H.ComponentHTML Action Slots monad
render state =
  case state.route of
    Nothing ->
      HH.text ""

    Just BreedsRoute ->
      HH.slot _breedsPage unit BreedsPage.component
        { breeds: state.breeds
        }
        HandleBreedsPage

    Just (ImagesRoute { breed, page }) ->
      HH.slot _imagesPage unit ImagesPage.component
        { breed
        , page
        , images:
            state.imagesCache
              # Map.lookup breed
              # maybe Loading Success
        }
        HandleImagesPage

handleAction ::
  forall output monad.
  MonadRouter Route monad =>
  MonadAff monad =>
  Action ->
  H.HalogenM State Action Slots output monad Unit
handleAction = case _ of
  Init -> do
    route <- HR.current
    H.modify_ \state -> state { route = route }

    emitter <- HR.emitMatched
    void $ H.subscribe (ChangeRoute <$> emitter)
    void $ H.fork $ do
      breeds <- BreedsApi.fetch
      H.modify_ \state -> state
        { breeds = Success breeds
        }

  ChangeRoute route -> do
    H.modify_ \state -> state { route = Just route }

  -- Fetches the images if we haven't selected this breed before
  -- Navigate to the images page for this breed
  HandleBreedsPage (BreedsPage.Selected breed) -> do
    HR.navigate $ ImagesRoute { breed, page: 1 }

    { imagesCache } <- H.get
    when (Map.lookup breed imagesCache == Nothing) $ void $ H.fork $ do
      images <- ImagesApi.fetch breed
      H.modify_ \state -> state
        { imagesCache = state.imagesCache # Map.insert breed images
        }

  HandleImagesPage ImagesPage.BackToBreeds ->
    HR.navigate BreedsRoute
