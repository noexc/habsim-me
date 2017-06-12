{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (when)
import qualified Data.Text as T
import Lucid
import Lucid.Base
import Web.Scotty
import Text.Digestive.Scotty
import Text.Digestive.View

-- Our modules
import HTML
import Form

formTemplate :: View T.Text -> ActionM ()
formTemplate view =
  html . renderText $ template "HABSim: Welcome" $ do
    div_ [ class_ "container" ] $ do
      div_ [ class_ "row" ] $ do
        div_ [ class_ "col-sm-12" ] $ do
          h1_ "Run a balloon simulation."
          hr_ []
          formHtml view
    script_ [] formTimeScript
    script_ [] formMap

main :: IO ()
main = scotty 3000 $ do
  get "/" $ do
    view <- getForm "sim" simForm
    formTemplate view
  post "/sim" $ do
    (view, result) <- runForm "sim" simForm
    case result of
      Just sf -> error "TODO!"
      _ -> formTemplate view
