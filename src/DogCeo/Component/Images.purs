module DogCeo.Component.Images
  ( component
  , Output(..)
  , Slot
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Set (Set)
import Data.Set as Set
import Debug (spy)
import DogCeo.Api.Utils as Api
import DogCeo.Routes (Route(..))
import DogCeo.Types (Breed)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Router.Class (class MonadRouter)
import Halogen.Router.Class as HR
import Record as Record

type Input = { | InputRow }
data Output = FetchImages

-- | The input is merged into the state, so we describe its properties in one place as a row type
type InputRow =
  ( breed :: Breed
  , page :: Int
  , images :: Api.Result (Array String)
  )

type Slot id = forall query. H.Slot query Output id

type State =
  { failedImageSources :: Set String
  | InputRow
  }

data Action
  = Init
  | Receive Input
  | NavBackToBreeds
  | GotoPreviousPage
  | GotoNextPage
  | ImageNotFound String

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
        { handleAction = spy "ImagesPage" >>> handleAction
        , initialize = Just Init
        , receive = Just <<< Receive
        }
    }

initialState :: Input -> State
initialState = Record.merge { failedImageSources: Set.empty }

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
            [ HP.class_ $ wrap "text-align" ]
            [ HH.text "An error occurred, contact support" ]

        Api.Success images ->
          HH.div [] $
            [ HH.div [ HP.class_ $ wrap "flex flex-row flex-wrap items-center justify-center gap-4" ] $
                pageImages { page: state.page, images } <#> \src ->
                  HH.img
                    [ HP.src src
                    , HE.onError \_ -> ImageNotFound src
                    , HP.class_ $ wrap $ Array.intercalate " "
                        [ "object-cover h-96 rounded"
                        , if src `Set.member` state.failedImageSources then "hidden" else ""
                        ]
                    ]
            , let
                failedImgCount =
                  pageImages { page: state.page, images }
                    # Array.filter (\src -> src `Set.member` state.failedImageSources)
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
      H.modify_ \state -> state { failedImageSources = state.failedImageSources # Set.insert src }

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
    Api.Loading -> 0
    Api.Error _ -> 0
    Api.Success array -> Array.length array

imageLimit :: Int
imageLimit = 20

pageImages :: { page :: Int, images :: Array String } -> Array String
pageImages { page, images } = Array.slice start end images
  where
  start = (page - 1) * imageLimit
  end = start + imageLimit
