{-# LANGUAGE OverloadedStrings #-}
module HTML
  ( template
  ) where

import qualified Data.Text as T
import Lucid
import Lucid.Base

navLink :: T.Text -> T.Text -> Html ()
navLink href text =
  li_ [ class_ "nav-item" ] $
    a_ [ class_ "nav-link", href_ href ] (toHtml text)

navbar :: Html ()
navbar =  do
  nav_ [ class_ "navbar navbar-toggleable-md navbar-inverse bg-primary" ] $ do
    button_ [ class_ "navbar-toggler navbar-toggler-right"
            , type_ "button"
            , makeAttribute "data-toggle" "collapse"
            , makeAttribute "data-target" "#navbarText"
            , makeAttribute "aria-controls" "navbarText"
            , makeAttribute "aria-expanded" "false"
            , makeAttribute "aria-label" "Toggle navigation"
            ] (span_ [ class_ "navbar-toggler-icon" ] "")
    a_ [ class_ "navbar-brand", href_ "/" ] "HABSim.me Mockup"
    div_ [ class_ "collapse navbar-collapse"
         , id_ "navbarText"
         ] $ do
      ul_ [ class_ "navbar-nav mr-auto" ] $ do
        navLink "https://github.com/kg4sgp/habsim-me" "source code"

template :: String -> Html () -> Html ()
template title body =
  doctypehtml_ $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      meta_ [ name_ "viewport"
            , content_ "width=device-width, initial-scale=1, shrink-to-fit=no"
            ]
      meta_ [makeAttribute "description" "High Altitude Ballon Simulator."]
      meta_ [makeAttribute "keywords" "habsim,hab,simulator,sim,balloon,ham,amateur,radio,flight"]
      title_ (toHtml title)
      link_ [ rel_ "stylesheet"
            , href_ "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-alpha.6/css/bootstrap.min.css"
            , makeAttribute "crossorigin" "anonymous"
            ]
      link_ [ rel_ "stylesheet"
            , href_ "https://cdnjs.cloudflare.com/ajax/libs/bootstrap-datetimepicker/4.17.47/css/bootstrap-datetimepicker.min.css"
            , makeAttribute "crossorigin" "anonymous"
            ]
      link_ [ rel_ "stylesheet"
            , href_ "https://fonts.googleapis.com/css?family=Ubuntu"
            , makeAttribute "crossorigin" "anonymous"
            ]
      style_ $
        "body { font-family: 'Ubuntu', sans-serif; }\
        \.navbar { margin-bottom: 20px; }\
        \html, body, #map {\
        \  margin: 0;\
        \  padding: 0;\
        \  height: 100%;\
        \}"
      script_ [ src_ "https://code.jquery.com/jquery-3.1.1.slim.min.js"
              , crossorigin_ "anonymous"
              ] ("" :: T.Text)
      script_ [ src_ "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-alpha.6/js/bootstrap.min.js"
              , crossorigin_ "anonymous"
              ] ("" :: T.Text)
      script_ [ src_ "https://cdnjs.cloudflare.com/ajax/libs/moment.js/2.18.1/moment.min.js"
              , crossorigin_ "anonymous"
              ] ("" :: T.Text)
      script_ [ src_ "https://cdnjs.cloudflare.com/ajax/libs/bootstrap-datetimepicker/4.17.47/js/bootstrap-datetimepicker.min.js"
              , crossorigin_ "anonymous"
              ] ("" :: T.Text)
    body_ $ do
      navbar
      body
      script_ [ src_ "https://maps.googleapis.com/maps/api/js?key=AIzaSyBExjRkrwviJVH9Lt9ZMHPq7CAH6gdI9gQ&callback=initMap" ] ("" :: T.Text)
