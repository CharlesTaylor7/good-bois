module DogCeo.Component.App
  ( component
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (wrap)
import DogCeo.Api.Images as ImagesApi
import DogCeo.Component.Breeds as ListView
import DogCeo.Component.Images as ImagesPage
import DogCeo.Types (ApiResult(..), Breed, Page(..))
import Effect.Aff (forkAff, runAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

type Slots =
  ( listView :: ListView.Slot
  , detailsView :: ImagesPage.Slot
  )

_listView = Proxy :: Proxy "listView"
_detailsView = Proxy :: Proxy "detailsView"

type State =
  { breeds :: ApiResult (Array Breed)
  , imagesCache :: Map String (Array String)
  , images :: ApiResult (Array String)
  , page :: Page
  }

data Action
  = HandleListView ListView.Output
  | HandleImagesPage ImagesPage.Output

component :: forall query input output monad. MonadAff monad => H.Component query input output monad
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        }
    }

initialState :: forall input. input -> State
initialState _ =
  { breeds: Loading
  , imagesCache: Map.empty
  , images: Loading
  , page: BreedsPage
  }

render :: forall monad. MonadAff monad => State -> H.ComponentHTML Action Slots monad
render state =
  HH.div
    [ HP.class_ $ wrap "mt-3 flex flex-col justify-center items-center" ]
    [ case state.page of
        BreedsPage ->
          HH.slot _listView unit ListView.component unit HandleListView
        ImagesPage { breed } ->
          HH.slot _detailsView unit ImagesPage.component { breed, images: state.images } HandleImagesPage
    ]

handleAction :: forall output monad. MonadAff monad => Action -> H.HalogenM State Action Slots output monad Unit
handleAction = case _ of
  HandleListView (ListView.Selected breed) -> do
    void $ H.fork $ do
      images <- ImagesApi.fetch breed
      H.modify_ \state -> state
        { images = Success images }

    H.modify_ \state -> state
      { page = ImagesPage { breed } }

  HandleImagesPage ImagesPage.ToListView ->
    H.modify_ \state -> state { page = BreedsPage }

{-
( case _ of
          Left error -> Console.error $ show error
          Right images -> H.modify_ \state -> state { images = images }
      )
-}
