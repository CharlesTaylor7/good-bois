module DogCeo.Component.App
  ( component
  ) where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Tuple.Nested ((/\))
import DogCeo.Types (ApiResult(..), Breed, Page(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Fetch (fetch)
import Fetch.Argonaut.Json as Json
import Foreign.Object as FO
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

type Slots =
  ( listView :: forall query. H.Slot query Void Int
  , detailsView :: forall query. H.Slot query Void Int
  )

_listView = Proxy :: Proxy "listView"
_detailsView = Proxy :: Proxy "detailsView"

type State =
  { counter :: Int
  , breeds :: ApiResult (Array Breed)
  , images :: Map String (Array String)
  , page :: Page
  }

data Action
  = Increment
  | FetchBreeds

component :: forall query input output monad. MonadAff monad => H.Component query input output monad
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just FetchBreeds
        }
    }

initialState :: forall input. input -> State
initialState _ =
  { counter: 0
  , breeds: Loading
  , images: Map.empty
  , page: BreedsPage
  }

render :: forall monad. State -> H.ComponentHTML Action () monad
render state =
  HH.div
    [ HP.class_ $ wrap "mt-3 flex flex-col justify-center items-center" ]
    [ case state.page of
        BreedsPage ->
          HH.text "Breeds - Coming soon"

        ImagesPage { breed } ->
          HH.div
            []
            [ HH.text "Images - Coming soon"
            , HH.text breed
            ]

    ]

-- button example
{-

let
  label = show state.counter
in
  hh.div
    [ hp.class_ $ wrap "mt-3 flex flex-col justify-center items-center" ]

    [ HH.button
        [ HP.title label
        , HE.onClick \_ -> Increment
        , HP.class_ $ wrap "border rounded-lg py-1 px-3 bg-sky-300"
        ]
        [ HH.text label ]
    ]
    -}

type DogBreedResponse =
  { message :: FO.Object (Array String)
  }

handleAction :: forall output monad. MonadAff monad => Action -> H.HalogenM State Action () output monad Unit
handleAction = case _ of
  Increment ->
    H.modify_ \state -> state { counter = state.counter + 1 }

  FetchBreeds -> do
    { message } <- liftAff $ do
      { json } <- fetch "https://dog.ceo/api/breeds/list/all" {}
      response :: DogBreedResponse <- Json.fromJson json
      pure response

    H.modify_ \state -> state
      { breeds =
          Success $
            (FO.toAscUnfoldable message :: Array _)
              <#> \(name /\ subBreeds) ->
                { name, subBreeds }
      }

