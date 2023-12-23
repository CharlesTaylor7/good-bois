module DogCeo.Routes
  ( Routes(..)
  , codec
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import DogCeo.Types (Breed)
import Routing.Duplex (RouteDuplex')
import Routing.Duplex as R
import Routing.Duplex.Generic as G
import Type.Proxy (Proxy(..))

data Routes
  = BreedsRoute
  | ImagesRoute { breed :: Breed, page :: Int }

derive instance Eq Routes
derive instance Generic Routes _

codec :: RouteDuplex' Routes
codec = R.root $ G.sum
  { "BreedsRoute": G.noArgs
  , "ImagesRoute": R.path "images" $ R.record
      # R.prop (Proxy :: _ "breed")
          ( R.record
              # R.prop (Proxy :: _ "name") (R.string R.segment)
              # R.prop (Proxy :: _ "subBreed") (R.optional R.segment)
          )
      # R.prop (Proxy :: _ "page") (R.int $ R.param "page")
  }

