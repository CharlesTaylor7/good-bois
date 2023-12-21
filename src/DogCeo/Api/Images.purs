module DogCeo.Api.Images
  ( fetch
  , Args
  ) where

import Prelude

import Data.Foldable (fold)
import Data.Maybe (Maybe, maybe)
import DogCeo.Types (ApiResult(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Fetch as Fetch
import Fetch.Argonaut.Json as Json

type Args =
  { breed :: String
  , subBreed :: Maybe String
  }

fetch :: forall monad. MonadAff monad => Args -> monad (ApiResult (Array String))
fetch { breed, subBreed } = liftAff $ do
  { json } <- Fetch.fetch url {}
  { message: images } :: DogImagesResponse <- Json.fromJson json
  pure $ Success images
  where
  url = fold
    [ "https://dog.ceo/api/breed/"
    , breed
    , subBreed # maybe "" ("/" <> _)
    , "/images"
    ]

type DogImagesResponse =
  { message :: Array String
  }
