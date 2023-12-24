module DogCeo.Component.Images
  ( component
  , Output(..)
  , Slot
  ) where

import Prelude

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (wrap)
import DogCeo.Api.Utils as Api
import DogCeo.Types (Breed)
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
  , images :: Api.Result (Array String)
  )

type Slot id = forall query. H.Slot query Output id

type State =
  { page :: Int
  , imageStatus :: Map String ImageLoad
  | InputRow
  }

data ImageLoad
  = LoadingImage
  | LoadedImage
  | ErrorImage

derive instance Eq ImageLoad

data Output = BackToBreeds

data Action
  = NavBackToBreeds
  | Receive Input
  | GotoPreviousPage
  | GotoNextPage
  | ImageLoaded String
  | ImageNotFound String

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
initialState = Record.merge
  { page: minPage
  , imageStatus: Map.empty
  }

render :: forall monad. State -> H.ComponentHTML Action () monad
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
                  Api.Loading -> ""
                  Api.Error _ -> ""
                  Api.Success images ->
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
                      Api.Loading -> ""
                      Api.Error _ -> ""
                      Api.Success _ -> Array.fold
                        [ "page "
                        , show state.page
                        , " of "
                        , show $ maxPage state
                        ]
                ]
            ]
        ]

    , case state.images of
        Api.Loading ->
          HH.div
            [ HP.class_ $ wrap "flex justify-center"
            ]
            [ HH.img
                [ HP.src "/static/loading.gif"
                , HP.alt "Loading"
                ]
            ]
        Api.Error _ ->
          HH.div
            [ HP.class_ $ wrap "text-center" ]
            [ HH.text "An error occurred, contact support" ]

        Api.Success images ->
          HH.div [] $
            [ HH.div [ HP.class_ $ wrap "flex flex-row flex-wrap items-center justify-center gap-4" ] $
                pageImages { page: state.page, images } <#> \src ->
                  HH.div []
                    [ HH.img
                        [ HP.src src
                        , HE.onLoad \_ -> ImageLoaded src
                        , HE.onError \_ -> ImageNotFound src
                        , HP.class_ $ wrap $ Array.intercalate " "
                            [ "object-cover h-96 w-96 rounded"

                            , if imgStatus src state /= LoadedImage then "hidden" else ""
                            ]
                        ]

                    , HH.img
                        [ HP.src "/static/loading.gif"
                        , HP.alt "Loading"
                        , HP.class_ $ wrap $ Array.intercalate " "
                            [ "object-cover h-96 w-96 rounded"
                            , if imgStatus src state /= LoadingImage then "hidden" else ""
                            ]
                        ]
                    , HH.div
                        [ HP.class_ $ wrap $ Array.intercalate " "
                            [ "flex h-96 w-96 rounded items-center justify-center"

                            , if imgStatus src state /= ErrorImage then "hidden" else ""
                            ]
                        ]
                        [ HH.img
                            [ HP.src "/static/image-not-found.png"
                            , HP.alt $ src <> " failed to load"
                            , HP.class_ $ wrap "h-32 w-32"
                            ]
                        ]

                    ]
            , let
                failedImgCount =
                  pageImages { page: state.page, images }
                    # Array.filter (\src -> imgStatus src state == ErrorImage)
                    # Array.length
              in
                HH.div
                  [ HP.class_ $ wrap $ Array.intercalate " "
                      [ "text-center"
                      , if failedImgCount == 0 then "hidden" else ""
                      ]
                  ]
                  [ HH.text $ show failedImgCount <> " images failed to load." ]

            --| Prefetches the next page of images
            --| Halogen's dom maniupulation keeps the same dom elements around and just swaps out the img src attributes. 
            --| This means after clicking 'Next', you can see stale images from the previous page while the new image is loading.
            --| Prefetching images allows images to swap out seemlessly after clicking the Next button.
            , HH.div [] $
                pageImages { page: state.page + 1, images } <#> \src ->
                  HH.link
                    [ HP.rel "prefetch"
                    , HP.href src
                    ]
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

imgStatus :: String -> State -> ImageLoad
imgStatus src state =
  state.imageStatus
    # Map.lookup src
    # fromMaybe LoadingImage

{-
  Pagination utilities
-}

minPage :: Int
minPage = 1

maxPage :: State -> Int
maxPage { images } =
  case images of
    Api.Loading -> 0
    Api.Error _ -> 0
    Api.Success array ->
      let
        n = Array.length array
        q = n `div` imagesPerPage
        r = n `mod` imagesPerPage
      in
        -- | division, but rounding up for a final extra page of less than 20 items
        q + if r > 0 then 1 else 0

imagesPerPage :: Int
imagesPerPage = 20

pageImages ::
  { page :: Int
  , images :: Array String
  } ->
  Array String
pageImages { page, images } =
  Array.slice start end images
  where
  start = (page - 1) * imagesPerPage
  end = start + imagesPerPage
