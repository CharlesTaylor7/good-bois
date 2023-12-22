module DogCeo.Component.Images
  ( component
  , Output(..)
  , Slot
  ) where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import DogCeo.Types (ApiResult(..), Breed)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Record as Record

type Input = Record InputRow

-- | The input is merged into the state, so we describe its properties in one place as a row type
type InputRow =
  ( breed :: Breed
  , images :: ApiResult (Array String)
  )

type Slot = forall query. H.Slot query Output Unit

type State =
  { page :: Int
  | InputRow
  }

data Output = BackToBreeds

data Action
  = NavBackToBreeds
  | Receive Input
  | GotoPreviousPage
  | GotoNextPage

component :: forall query monad. MonadAff monad => H.Component query Input Output monad
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }

initialState :: Input -> State
initialState input = Record.merge input { page: 1 }

render :: forall monad. State -> H.ComponentHTML Action () monad
render state =
  HH.div
    []
    [ HH.div [ HP.class_ $ wrap "m-6 flex flex-row items-center justify-evenly " ]
        [ HH.a
            [ HP.class_ $ wrap "flex items-center p-2 cursor-pointer underline decoration-blue-400 text-sky-500"
            , HE.onClick \_ -> NavBackToBreeds
            ]
            [ HH.text "Back to Breeds" ]

        , HH.div
            [ HP.class_ $ wrap "flex flex-col items-center" ]
            [ HH.h2
                [ HP.class_ $ wrap "text-xl font-semibold capitalize" ]
                [ HH.text $
                    case state.breed.subBreed of
                      Just subBreed -> subBreed <> " " <> state.breed.name
                      Nothing -> state.breed.name
                ]
            , HH.text $
                case state.images of
                  Loading -> ""
                  Success images ->
                    show (Array.length images) <> " images"
            ]

        , HH.div [ HP.class_ $ wrap "text-center" ] $
            [ HH.div
                [ HP.class_ $ wrap "flex flex-row items-center gap-2" ]
                [ HH.button
                    [ buttonStyle
                    , HP.disabled $ state.page <= minPage
                    , HE.onClick \_ -> GotoPreviousPage
                    ]
                    [ HH.text "Previous" ]

                , HH.button
                    [ buttonStyle
                    , HP.disabled $ state.page >= maxPage state
                    , HE.onClick \_ -> GotoNextPage
                    ]
                    [ HH.text "Next" ]

                , HH.text $
                    case state.images of
                      Loading -> ""
                      Success _ -> Array.fold
                        [ "page "
                        , show state.page
                        , " of "
                        , show $ maxPage state
                        ]
                ]
            ]
        ]

    , case state.images of
        Loading ->
          HH.div
            [ HP.class_ $ wrap "flex justify-center"
            ]
            [ HH.img
                [ HP.src "./static/loading.gif"
                , HP.alt "Loading"
                ]
            ]

        Success images ->
          HH.div [ HP.class_ $ wrap "flex flex-row flex-wrap items-center justify-center gap-4" ] $
            currentPageImages { page: state.page, images } <#> \src ->
              HH.img
                [ HP.src src
                , HP.class_ $ wrap "object-cover h-96 rounded"
                ]
    ]
  where
  buttonStyle = HP.class_ $ wrap "border rounded-lg py-2 px-4 bg-sky-300 disabled:bg-slate-200"

handleAction :: forall slots monad. MonadAff monad => Action -> H.HalogenM State Action slots Output monad Unit
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

{-
  Pagination utilities
-}

minPage :: Int
minPage = 1

maxPage :: State -> Int
maxPage { images } =
  let
    q = n `Int.quot` imageLimit
    r = n `Int.rem` imageLimit
  in
    -- | division, but rounding up for a final extra page of less than 20 items
    q + if r > 0 then 1 else 0

  where
  n = case images of
    Loading -> 0
    Success array -> Array.length array

imageLimit :: Int
imageLimit = 20

currentPageImages ::
  { page :: Int
  , images :: Array String
  } ->
  Array String
currentPageImages { page, images } =
  Array.slice start end images
  where
  -- Note: the pages are indexed from 1
  start = (page - 1) * 20
  end = page * 20
