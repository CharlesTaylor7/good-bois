module DogCeo.Component.App
  ( component
  ) where

import Prelude

import Data.Foldable (for_)
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

type State =
  { breeds :: ApiResult (Array BreedGroup)
  , imagesCache :: Map Breed (Array String)
  , route :: Route
  }

data Action
  = Init
  | RouteChanged Route
  | HandleImagesPage Breed ImagesPage.Output

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
  , route: BreedsRoute
  }

render ::
  forall monad.
  MonadRouter Route monad =>
  MonadAff monad =>
  State ->
  H.ComponentHTML Action Slots monad
render state =
  case state.route of
    BreedsRoute ->
      HH.slot_
        (Proxy :: _ "breedsPage")
        unit
        BreedsPage.component
        { breeds: state.breeds
        }

    ImagesRoute { breed, page } ->
      HH.slot
        (Proxy :: _ "imagesPage")
        unit
        ImagesPage.component
        { breed
        , page
        , images:
            state.imagesCache
              # Map.lookup breed
              # maybe Loading Success
        }
        (HandleImagesPage breed)

handleAction ::
  forall output monad.
  MonadRouter Route monad =>
  MonadAff monad =>
  Action ->
  H.HalogenM State Action Slots output monad Unit
handleAction = case _ of
  Init -> do
    currentRoute <- HR.current
    for_ currentRoute $ \route ->
      H.modify_ \state -> state { route = route }

    emitter <- HR.emitMatched
    void $ H.subscribe (RouteChanged <$> emitter)
    void $ H.fork $ do
      breeds <- BreedsApi.fetch
      H.modify_ \state -> state
        { breeds = Success breeds
        }

  RouteChanged route -> do
    H.modify_ \state -> state { route = route }

  HandleImagesPage breed ImagesPage.FetchImages -> do
    images <- ImagesApi.fetch breed
    H.modify_ \state -> state
      { imagesCache = state.imagesCache # Map.insert breed images
      }
