module DogCeo.Component.Images.View
  ( render
  ) where

import Prelude

import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (wrap)
import DogCeo.Api.Utils as Api
import DogCeo.Component.Images.Types (Action(..), ImageLoad(..), State)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

render :: forall monad. State -> H.ComponentHTML Action () monad
render state =
  HH.div
    []
    [ renderHeader state
    , renderImages state
    ]

renderHeader :: forall monad. State -> H.ComponentHTML Action () monad
renderHeader state =
  HH.div [ HP.class_ $ wrap "m-6 flex flex-row flex-wrap items-center justify-evenly " ]
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

  where
  buttonStyle = HP.class_ $ wrap "border rounded-lg py-2 px-4 bg-sky-300 disabled:bg-slate-200"

renderImages :: forall monad. State -> H.ComponentHTML Action () monad
renderImages state =
  case state.images of
    Api.Loading ->
      HH.div
        [ HP.class_ $ wrap "flex justify-center"
        ]
        [ HH.img
            [ HP.src "/good-bois/static/loading.gif"
            , HP.alt "Loading"
            ]
        ]
    Api.Error _ ->
      HH.div
        [ HP.class_ $ wrap "text-center" ]
        [ HH.text "An error occurred, contact support" ]

    Api.Success images ->
      HH.div [] $
        [ HH.div [ HP.class_ $ wrap "flex flex-row flex-wrap items-center justify-center gap-4" ]
            $ pageImages { page: state.page, images }
                <#> \src ->
                  HH.div [ HP.class_ $ wrap "relative h-96 w-96" ]
                    [ HH.img
                        [ HP.src src
                        , HE.onLoad \_ -> ImageLoaded src
                        , HE.onError \_ -> ImageNotFound src
                        , HP.class_ $ wrap $ Array.intercalate " "
                            [ "absolute h-full w-full object-cover rounded transition-opacity"
                            , if imgStatus src state /= LoadedImage then "opacity-0" else "opacity-100"
                            ]
                        ]
                    , HH.img
                        [ HP.src "/good-bois/static/loading.gif"
                        , HP.alt "Loading"
                        , HP.class_ $ wrap $ Array.intercalate " "
                            [ "absolute h-full w-full object-cover rounded transition-opacity"
                            , if imgStatus src state /= LoadingImage then "opacity-0" else "opacity-100"
                            ]
                        ]
                    , HH.div
                        [ HP.class_ $ wrap $ Array.intercalate " "
                            [ "absolute h-full w-full flex rounded items-center justify-center transition-opacity"
                            , if imgStatus src state /= ErrorImage then "opacity-0" else "opacity-100"
                            ]
                        ]
                        [ HH.img
                            [ HP.src "/good-bois/static/image-not-found.png"
                            , HP.alt $ src <> " failed to load"
                            , HP.class_ $ wrap "h-32 w-32"
                            ]
                        ]

                    ]
        --| Prefetches the next page of images
        --| Halogen's dom maniupulation keeps the same dom elements around and just swaps out the img src attributes. 
        --| This means after clicking 'Next', you can see stale images from the previous page while the new image is loading.
        --| Prefetching images allows images to swap out seemlessly after clicking the Next button.
        , HH.div []
            $ pageImages { page: state.page + 1, images }
                <#> \src ->
                  HH.link
                    [ HP.rel "prefetch"
                    , HP.href src
                    ]
        ]

{-
  Pagination utilities
-}

imgStatus :: String -> State -> ImageLoad
imgStatus src state =
  state.imageStatus
    # Map.lookup src
    # fromMaybe LoadingImage

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
