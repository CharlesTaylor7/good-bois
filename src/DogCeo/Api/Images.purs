module DogCeo.Api.Images
  ( fetch
  ) where

import Prelude

import Data.Foldable (fold)
import Data.Maybe (Maybe, maybe)
import DogCeo.Types (ApiResult(..), Breed)
import Effect.Aff.Class (class MonadAff, liftAff)
import Fetch as Fetch
import Fetch.Argonaut.Json as Json

fetch :: forall monad. MonadAff monad => Breed -> monad (ApiResult (Array String))
fetch { name, subBreed } = liftAff $ do
  { json } <- Fetch.fetch url {}
  { message: images } :: DogImagesResponse <- Json.fromJson json
  pure $ Success images
  where
  url = fold
    [ "https://dog.ceo/api/breed/"
    , name
    , subBreed # maybe "" ("/" <> _)
    , "/images"
    ]

type DogImagesResponse =
  { message :: Array String
  }
