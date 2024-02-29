module DogCeo.Component.Image
  ( component
  , Slot
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (wrap)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Slot id = forall query. H.Slot query Void id
type Input = { url :: String }

type State =
  { previous :: Maybe String
  , current :: String
  , status :: ImageLoad
  }

data Action
  = Init
  | Receive Input
  | ImageNotFound
  | ImageLoaded

component ::
  forall query output monad.
  H.Component query Input output monad
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
initialState { url } =
  { previous: Nothing
  , current: url
  , status: LoadingImage
  }

render ::
  forall monad slots.
  State ->
  H.ComponentHTML Action slots monad
render state =
  HH.div
    [ HP.class_ $ wrap "relative h-96 w-96" ]
    [ HH.img
        [ HP.src state.current
        , HE.onLoad \_ -> ImageLoaded
        , HE.onError \_ -> ImageNotFound
        , HP.class_ $ wrap $ Array.intercalate " "
            [ "absolute h-full w-full object-cover rounded transition-opacity"
            , if state.status /= LoadedImage then "opacity-0" else "opacity-100"
            ]
        ]
    , HH.img
        [ HP.src "/good-bois/static/loading.gif"
        , HP.alt "Loading"
        , HP.class_ $ wrap $ Array.intercalate " "
            [ "absolute h-full w-full object-cover rounded transition-opacity"
            , if state.status /= LoadingImage then "opacity-0" else "opacity-100"
            ]
        ]
    , HH.div
        [ HP.class_ $ wrap $ Array.intercalate " "
            [ "absolute h-full w-full flex rounded items-center justify-center transition-opacity"
            , if state.status /= ErrorImage then "opacity-0" else "opacity-100"
            ]
        ]
        [ HH.img
            [ HP.src "/good-bois/static/image-not-found.png"
            , HP.alt $ state.current <> " failed to load"
            , HP.class_ $ wrap "h-32 w-32"
            ]
        ]
    ]

handleAction ::
  forall output monad slots.
  Action ->
  H.HalogenM State Action slots output monad Unit
handleAction = case _ of
  Init -> pure unit
  Receive { url } ->
    H.modify_ $ \state -> (state { current = url, previous = Just state.current, status = LoadingImage })
  ImageNotFound ->
    H.modify_ $ (_ { status = ErrorImage })
  ImageLoaded ->
    H.modify_ $ (_ { status = LoadedImage })

data ImageLoad
  = LoadingImage
  | LoadedImage
  | ErrorImage

derive instance Eq ImageLoad
