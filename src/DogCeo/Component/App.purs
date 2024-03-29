-- | This component renders no html directly.
-- | It performing api calls, and caching previous calls to the breeds or images api. 
-- | It tracks the current page and delegates rendering to the relevant child component.
module DogCeo.Component.App
  ( component
  ) where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import DogCeo.Api.Breeds as BreedsApi
import DogCeo.Api.Images as ImagesApi
import DogCeo.Api.Utils as Api
import DogCeo.Component.Breeds as BreedsPage
import DogCeo.Component.Images as ImagesPage
import DogCeo.Routes (Route(..))
import DogCeo.Types (Breed, BreedGroup)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Router.Class (class MonadRouter)
import Halogen.Router.Class as HR
import Type.Proxy (Proxy(..))

type Slots =
  ( breedsPage :: BreedsPage.Slot Unit
  , imagesPage :: ImagesPage.Slot Unit
  )

type State =
  { breeds :: Api.Result (Array BreedGroup)
  , imagesCache :: Map Breed (Api.Result (Array String))
  , route :: Maybe Route
  }

data Action
  = Init
  | RouteChanged Route
  | HandleBreedsPage BreedsPage.Output
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
  { breeds: Api.Loading
  , imagesCache: Map.empty
  , route: Nothing
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
      HH.slot
        (Proxy :: _ "breedsPage")
        unit
        BreedsPage.component
        { breeds: state.breeds
        }
        HandleBreedsPage

    Just (ImagesRoute { breed, page }) ->
      HH.slot
        (Proxy :: _ "imagesPage")
        unit
        ImagesPage.component
        { breed
        , page
        , images: state # lookupImages breed
        }
        (HandleImagesPage breed)

lookupImages :: Breed -> State -> Api.Result (Array String)
lookupImages breed state =
  state.imagesCache
    # Map.lookup breed
    # fromMaybe Api.Loading

handleAction ::
  forall output monad.
  MonadRouter Route monad =>
  MonadAff monad =>
  Action ->
  H.HalogenM State Action Slots output monad Unit
handleAction = case _ of
  Init -> do
    route <- HR.current
    H.modify_ \state -> state
      { route = Just $ (route # fromMaybe BreedsRoute) }

    emitter <- HR.emitMatched
    void $ H.subscribe (RouteChanged <$> emitter)

  RouteChanged route -> do
    H.modify_ \state -> state { route = Just route }

  HandleBreedsPage BreedsPage.FetchBreeds -> do
    breeds <- BreedsApi.fetch
    H.modify_ \state -> state { breeds = breeds }

  -- Fetches the images if we haven't selected this breed before
  -- Navigate to the images page for this breed
  HandleImagesPage breed ImagesPage.FetchImages -> do
    images <- ImagesApi.fetch breed

    -- liftAff $ delay $ Milliseconds 400_000.0

    H.modify_ \state -> state
      { imagesCache = state.imagesCache # Map.insert breed images
      }

