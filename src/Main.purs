module Main where

import Prelude

import DogCeo.Component.App as App
import DogCeo.Routes as Routes
import Effect (Effect)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.Router.Trans.PushState (mkRouter, runRouterT)
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  router <- liftEffect $ mkRouter Routes.codec
  let rootComponent = H.hoist (runRouterT router) App.component
  runUI rootComponent unit body

