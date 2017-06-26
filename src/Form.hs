{-# LANGUAGE OverloadedStrings #-}
module Form where

import Data.HABSim.Types
import qualified Data.Text as T
import Data.Time.LocalTime (utc)
import Lucid
import Lucid.Base
import Text.Digestive

import Types

simForm :: Monad m => Form T.Text m SimForm
simForm =
  SimForm
  <$> "lat" .: stringRead "Invalid latitude" Nothing
  <*> "lon" .: stringRead "Invalid longitude" Nothing
  <*> "alt" .: stringRead "Invalid altitude" Nothing
  <*> "time" .: utcTimeFormlet "%F" "%T" utc Nothing
  <*> "mass" .: stringRead "Invalid mass" Nothing
  <*> "ascent_rate" .: stringRead "Invalid ascent rate" Nothing
  <*> "burst_volume" .: stringRead "Invalid burst volume" Nothing
  <*> "initial_fill_volume" .: stringRead "Invalid initial fill volume" Nothing
  <*> "balloon_coeff_drag" .: stringRead "Invalid balloon coefficient of drag" Nothing
  <*> "parachute_coeff_drag" .: stringRead "Invalid parachute coefficient of drag" Nothing
  <*> "parachute_area" .: stringRead "Invalid parachute area" Nothing
  <*> "package_coeff_drag" .: stringRead "Invalid package coefficient of drag" Nothing
  <*> "grib_url" .: gribUrlCheck
  where
    gribUrlCheck =
      check
      "Must start with 'http://nomads.ncep.noaa.gov/'"
      ("http://nomads.ncep.noaa.gov/" `T.isPrefixOf`) $ text Nothing

formError :: [T.Text] -> Html ()
formError errors =
  case length errors of
    0 -> return ()
    1 -> div_ [class_ "alert alert-danger", role_ "alert"] $
      (toHtml . head $ errors)
    _ -> div_ [class_ "alert alert-danger", role_ "alert"] $ do
           ul_ [] (mapM_ (li_ [] . toHtml) errors)

formHtml :: View T.Text -> Html ()
formHtml view = do
  form_ [ action_ "/sim", method_ "POST" ] $ do
    formError (childErrors "time.date" view)
    input_ [ type_ "hidden"
           , name_ "sim.time.date"
           , id_ "sim-time-date"
           , value_ (fieldInputText "time.date" view) ]
    input_ [ type_ "hidden"
           , name_ "sim.time.time"
           , id_ "sim-time-time"
           , value_ (fieldInputText "time.time" view) ]
    h4_ "First off, when do you plan to launch?"
    p_ "Please use UTC in your selection."
    div_ [ id_ "dp" ] ""
    hr_ []
    h4_ "Cool, now we need some more information..."
    p_ $ do
      "Tell us, from where do you plan on launching? "
      "You can click on the map below to place a pin."
    formError (errors "lat" view)
    formError (errors "lon" view)
    formError (errors "alt" view)
    div_ [ id_ "map", style_ "width: 100%; height: 330px;" ] ""
    input_ [ type_ "text"
           , name_ "sim.lat"
           , id_ "sim-lat"
           , value_ (fieldInputText "lat" view) ]
    input_ [ type_ "text"
           , name_ "sim.lon"
           , id_ "sim-lon"
           , value_ (fieldInputText "lon" view) ]
    p_ "From what altitude will you be launching?"
    input_ [ type_ "text"
           , name_ "sim.alt"
           , id_ "sim-alt"
           , value_ (fieldInputText "alt" view) ]
    hr_ []
    p_ "Gotcha. Now tell us a little about your balloon."
    formError (errors "mass" view)
    formError (errors "ascent_rate" view)
    formError (errors "burst_volume" view)
    formError (errors "initial_fill_volume" view)
    formError (errors "balloon_coeff_drag" view)
    p_ "What is its mass (in kilograms)?"
    input_ [ type_ "text"
           , name_ "sim.mass"
           , id_ "sim-mass"
           , value_ (fieldInputText "mass" view) ]
    p_ "What is its ascent rate (in meters per second)?"
    input_ [ type_ "text"
           , name_ "sim.ascent_rate"
           , id_ "sim-ascent_rate"
           , value_ (fieldInputText "ascent_rate" view) ]
    p_ "What is its volume, when it bursts?"
    input_ [ type_ "text"
           , name_ "sim.burst_volume"
           , id_ "sim-burst_volume"
           , value_ (fieldInputText "burst_volume" view) ]
    p_ "What is its volume at launch time?"
    input_ [ type_ "text"
           , name_ "sim.initial_fill_volume"
           , id_ "sim-initial_fill_volume"
           , value_ (fieldInputText "initial_fill_volume" view) ]
    p_ "What is its coefficient of drag?"
    input_ [ type_ "text"
           , name_ "sim.balloon_coeff_drag"
           , id_ "sim-balloon_coeff_drag"
           , value_ (fieldInputText "balloon_coeff_drag" view) ]
    hr_ []
    h4_ "Do you have a parachute?"
    p_ "If you don't, set these to 0."
    formError (errors "parachute_coeff_drag" view)
    formError (errors "parachute_area" view)
    p_ "What is the parachute's coefficient of drag?"
    input_ [ type_ "text"
           , name_ "sim.parachute_coeff_drag"
           , id_ "sim-parachute_coeff_drag"
           , value_ (fieldInputText "parachute_coeff_drag" view) ]
    p_ "What is the parachute's area (in meters)?"
    input_ [ type_ "text"
           , name_ "sim.parachute_area"
           , id_ "sim-parachute_area"
           , value_ (fieldInputText "parachute_area" view) ]
    hr_ []
    h4_ "Now, your payload."
    formError (errors "package_coeff_drag" view)
    p_ "What is your payload's coefficient of drag?"
    input_ [ type_ "text"
           , name_ "sim.package_coeff_drag"
           , id_ "sim-package_coeff_drag"
           , value_ (fieldInputText "package_coeff_drag" view) ]
    hr_ []
    h4_ "Blown away by how cool this is?"
    formError (errors "grib_url" view)
    p_ $ do
      "We need to where to get wind data from. In the future, this will be an "
      "automatic process, but for now, we require a URL to GRIB data from "
      "NOAA. Please grab the URL to a GRIB file from "
      a_
        [ href_ "http://nomads.ncep.noaa.gov/cgi-bin/filter_gfs_0p25.pl" ]
        "here"
      " and paste it into the box."
    p_ "Note: Only paste a URL, do not paste the GRIB data itself."
    input_ [ type_ "text"
           , name_ "sim.grib_url"
           , id_ "sim-grib_url"
           , value_ (fieldInputText "grib_url" view) ]
    p_ "That's it!"
    input_ [ type_ "submit"
           , value_ "Run simulation"
           ]

formTimeScript :: T.Text
formTimeScript =
  "$(function() {\
  \  def_date = new Date();\
  \  if ($('#sim-time-date').val() != '' && $('#sim-time-time').val() != '') {\
  \    def_date = $('#sim-time-date').val() + ' ' + $('#sim-time-time').val();\
  \  }\
  \  $('#dp').datetimepicker({\
  \    inline: true,\
  \    sideBySide: true,\
  \    format: 'YYYY-MM-DD HH:mm:ss',\
  \    defaultDate: def_date\
  \  });\
  \  $('#dp').on('dp.change', function(e) {\
  \    $('#sim-time-date').val(e.date.format('YYYY-MM-DD'));\
  \    $('#sim-time-time').val(e.date.format('HH:mm:ss'));\
  \  });\
  \});"

formMap :: T.Text
formMap =
  "var marker = null;\
  \function markerAndPanTo(latLng, map) {\
  \  if (marker != null) { marker.setMap(null); }\
  \  marker = new google.maps.Marker({\
  \    position: latLng,\
  \    map: map\
  \  });\
  \  $('#sim-lat').val(latLng.lat);\
  \  $('#sim-lon').val(latLng.lng);\
  \  /*map.panTo(latLng);*/\
  \}\
  \function initMap() {\
  \  var myLatlng = {lat: 40.363, lng: -80.044};\
  \  var map = new google.maps.Map(document.getElementById('map'), {\
  \    zoom: 4,\
  \    center: myLatlng\
  \  });\
  \  map.addListener('click', function(e) {\
  \    markerAndPanTo(e.latLng, map);\
  \  });\
  \}"
