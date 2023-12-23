module DogCeo.Component.Images
  ( component
  , Output(..)
  , Slot
  ) where

import Prelude

import Data.Array as Array
import Data.Enum (enumFromTo)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import DogCeo.Component.Image as Image
import DogCeo.Routes (Route(..))
import DogCeo.Types (ApiResult(..), Breed)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Router.Class (class MonadRouter)
import Halogen.Router.Class as HR
import Record as Record
import Type.Proxy (Proxy(..))

type Input = { | InputRow }
data Output = FetchImages

-- | The input is merged into the state, so we describe its properties in one place as a row type
type InputRow =
  ( breed :: Breed
  , page :: Int
  , images :: ApiResult (Array String)
  )

type Slot = forall query. H.Slot query Output Unit
type Slots = (image :: Image.Slot)

type State = { | InputRow }

data Action
  = Init
  | Receive Input
  | NavBackToBreeds
  | GotoPreviousPage
  | GotoNextPage

component ::
  forall query monad.
  MonadRouter Route monad =>
  MonadAff monad =>
  H.Component query Input Output monad
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Init
        , receive = Just <<< Receive
        }
    }

initialState :: Input -> State
initialState = Record.merge {}

render :: forall monad. State -> H.ComponentHTML Action Slots monad
render state =
  HH.div
    []
    [ HH.div [ HP.class_ $ wrap "m-6 flex flex-row flex-wrap items-center justify-evenly " ]
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
            renderPageImages state images
    ]

  where
  buttonStyle = HP.class_ $ wrap "border rounded-lg py-2 px-4 bg-sky-300 disabled:bg-slate-200"

handleAction ::
  forall monad.
  MonadRouter Route monad =>
  MonadAff monad =>
  Action ->
  H.HalogenM State Action Slots Output monad Unit
handleAction =
  case _ of
    Init ->
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

{-
  Pagination utilities
-}

minPage :: Int
minPage = 1

maxPage :: State -> Int
maxPage { images } =
  let
    q = n `div` imageLimit
    r = n `mod` imageLimit
  in
    -- | division, but rounding up for a final extra page of less than 20 items
    q + if r > 0 then 1 else 0

  where
  n = case images of
    Loading -> 0
    Success array -> Array.length array

imageLimit :: Int
imageLimit = 20

-- | Each image is rendered alongside a <link> tag which prefetches the image which is exactly 20 indices ahead of it.
-- | Halogen's dom maniupulation keeps the same dom elements around and just swaps out the img src. This means after clicking 'Next', you can see stale images from the previous page while the new image is loading.
-- | Prefetching images allows images to swap out seemlessly after clicking the Next button.
renderPageImages ::
  forall action monad.
  State ->
  Array String ->
  Array (HH.ComponentHTML action Slots monad)
renderPageImages { page } images =
  enumFromTo 1 imageLimit
    # Array.mapMaybe
        \i ->
          let
            currentIndex = (page - 1) * imageLimit + (i - 1)
            nextIndex = currentIndex + imageLimit
            next = Array.index images nextIndex

          in
            Array.index images currentIndex <#> \current ->
              HH.slot_ (Proxy :: _ "image") i Image.component { current, next }
