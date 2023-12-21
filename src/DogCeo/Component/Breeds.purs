module DogCeo.Component.Breeds
  ( component
  ) where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Tuple.Nested ((/\))
import DogCeo.Api.Breeds as BreedsApi
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

type State =
  { breeds :: ApiResult (Array Breed)
  }

data Action = FetchBreeds

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
  { breeds: Loading
  }

render :: forall monad. State -> H.ComponentHTML Action () monad
render state =
  HH.div
    []
    [ HH.h2 [ HP.class_ $ wrap "text-xl font-semibold" ] [ HH.text "Dog Breeds" ]
    , case state.breeds of
        Loading ->
          HH.text "Loading..."

        Success breeds ->
          HH.ul [ HP.class_ $ wrap "pl-4 list-disc" ] $
            breeds <#> \breed ->
              HH.li []
                [ HH.text breed.name
                , HH.ul [] $
                    breed.subBreeds <#> \name ->
                      HH.li [] [ HH.text name ]
                ]
    ]

{-

          <h2 class="text-xl font-semibold">Dog Breeds</h2>
          <ul class="pl-4 list-disc">
              ~children~
          </ul>
 
<li>
              <a 
                class="flex items-center p-2 cursor-pointer underline decoration-blue-400 text-sky-500"
                ~onClick~
              >
                ~name~
             

              <ul class="pl-4 list-disc">
                ~subBreeds~
              </ul>
   -}
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
  FetchBreeds -> do
    breeds <- BreedsApi.fetch
    H.modify_ \state -> state
      { breeds = breeds }
