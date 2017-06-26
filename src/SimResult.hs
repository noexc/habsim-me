{-# LANGUAGE OverloadedStrings #-}
module SimResult where

import Control.Lens
import Control.Monad.Writer
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.DList as D
import Data.HABSim.HABSim (sim)
import Data.HABSim.Grib2.CSVParse
import Data.HABSim.Internal (altToPressure)
import Data.HABSim.Lens
import Data.HABSim.Types
import Data.HABSim.VectorUtilities
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import Data.Time
import Data.Time.LocalTime (utc)
import qualified Data.Vector as V
import Lucid
import Lucid.Base
import Text.Digestive

import Types

getGribLines :: BL.ByteString -> HM.HashMap Key GribLine
getGribLines csv = either error keyedGribToHM (decodeKeyedGrib csv)

runSim :: HM.HashMap Key GribLine -> SimForm -> T.Text
runSim gribLines (SimForm lat lon alt _ blMass ascent bVol blVol blCoD prCoD _ pkgCoD _) =
  let sv = SimulationTime 0.1 0.0
      pv = PosVel lat                -- lat (decimal degrees)
                  lon                -- lon (decimal degrees)
                  (Altitude alt)     -- altitude (m)
                  0.0                -- velocity x initial (m/s)
                  0.0                -- velocity y initial (m/s)
                  (Velocity ascent)  -- velocity z (ascent rate) (m/s)
      bv = Burst  (Mass blMass)      -- mass (kg)
                  (CoeffDrag blCoD)  -- Balloon Coefficent of Drag
                  (CoeffDrag prCoD)  -- Parachute Coefficent of Drag
                  (CoeffDrag pkgCoD) -- Packages Coefficent of Drag
                  0.0                -- Launch Time (Not used currently)
                  (Liter bVol)       -- Burst Volume (Liters)
                  (Liter blVol)      -- Balloon Volume initial (Liters)
                  ((altToPressure (Altitude alt)) ^. pressure)
                  -- ^ Balloon Pressure initial (Pascals)
                  2.01588E-3        -- Lifting gas molar mass (hydrogen)
                  288.15            -- Lifting gas temperature
      w = Wind 0 0
      s = Simulation sv pv bv w
      tellPred simul =
        round (_simulationTime (_retSV simul)) `mod` 100 == (0 :: Integer)
      pressures =
        nub
        (fmap (\x -> gribLineToRaw x ^. pressure)
        (V.fromList . HM.elems $ gribLines))
      asc@(lastAscent, _) =
        runWriter $ sim Ascent s pressures gribLines tellPred
      ascentLastSim =
        Simulation
        (lastAscent ^. retSV)
        (lastAscent ^. retPV)
        (lastAscent ^. retBV)
        (lastAscent ^. retW)
      desc =
        runWriter $ sim Descent ascentLastSim pressures gribLines tellPred
  in toJSArray asc desc

jsonLatLon :: Simulation -> T.Text
jsonLatLon (Simulation _ (PosVel lat' lon' _ _ _ _) _ _) =
  "{lat: " <> T.pack (show lat') <> ", lng: " <> T.pack (show lon') <> "}"

toJSArray
  :: (Simulation, D.DList Simulation)
  -> (Simulation, D.DList Simulation)
  -> T.Text
toJSArray (lastAscent, accAscent) (lastDescent, accDescent) =
  "var burst_point = " <> jsonLatLon lastAscent <> ";" <>
  "var flight_path = [" <>
  (T.intercalate "," . map jsonLatLon . D.toList $ accAscent) <>
  "," <> jsonLatLon lastAscent <>  "," <>
  (T.intercalate "," . map jsonLatLon . D.toList $ accDescent) <>
  "," <> jsonLatLon lastDescent <> "];"

renderSim :: T.Text -> Html ()
renderSim js = do
    div_ [ id_ "map", style_ "width: 100%; height: 100%;" ] ""
    script_ [ type_"text/javascript" ] (toHtmlRaw $ gmapJS js)

{-
gmapTemplate :: Html ()
gmapTemplate =
  div_ [ id_ "map", style_ "width: 100%; height: 100%;" ] ""
-}

gmapJS :: T.Text -> T.Text
gmapJS simResult =
  simResult <> "\n" <>
  "function initMap() {\
  \  var map = new google.maps.Map(document.getElementById('map'), {\
  \    zoom: 7,\
  \    center: burst_point,\
  \    mapTypeId: 'terrain'\
  \  });\
  \  var burst_marker = new google.maps.Marker({\
  \    position: burst_point,\
  \    map: map\
  \  });\
  \  var flightPath = new google.maps.Polyline({\
  \    path: flight_path,\
  \    geodesic: false,\
  \    strokeColor: '#FF0000',\
  \    strokeOpacity: 1.0,\
  \    strokeWeight: 2,\
  \    map: map\
  \  });\
  \}"
