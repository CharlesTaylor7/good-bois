module DogCeo.Api.Images
  ( fetch
  ) where

import Prelude

import Data.Foldable (fold)
import Data.Maybe (maybe)
import DogCeo.Api.Utils as Api
import DogCeo.Types (Breed)
import Effect.Aff.Class (class MonadAff, liftAff)
import Fetch as Fetch
import Fetch.Argonaut.Json as Json

fetch :: forall monad. MonadAff monad => Breed -> monad (Api.Result (Array String))
fetch { name, subBreed } = liftAff $ Api.attempt $ do
  { json } <- Fetch.fetch url {}
  { message: images } :: DogImagesResponse <- Json.fromJson json
  pure images
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
