{-# LANGUAGE OverloadedStrings #-}
module Types where
import Data.Aeson
import qualified Data.Text as T
import Data.Time

-- TODO: Would like to use better types here, but d-f stringRead seems to hate
-- it.
data SimForm =
  SimForm { fLat :: Double
          , fLon :: Double
          , fAlt :: Double
          , fTime :: UTCTime
          , fMass :: Double
          , fAscentRate :: Double
          , fBurstVolume :: Double
          , fInitialFillVolume :: Double
          , fBalloonCoeffDrag :: Double
          , fParachuteCoeffDrag :: Double
          , fParachuteArea :: Double
          , fPackageCoeffDrag :: Double
          , fGribUrl :: T.Text
          } deriving (Eq, Show)

instance ToJSON SimForm where
  toJSON (SimForm lat lon alt time blMass ascent bVol blVol blCoD prCoD prArea pkgCoD gribUrl) =
    object [ "lat" .= lat
           , "lon" .= lon
           , "alt" .= alt
           , "time" .= time
           , "balloon_mass" .= blMass
           , "ascent_rate" .= ascent
           , "burst_volume" .= bVol
           , "balloon_volume" .= blVol
           , "balloon_coeff_drag" .= blCoD
           , "parachute_coeff_drag" .= prCoD
           , "parachute_area" .= prArea
           , "package_coeff_drag" .= pkgCoD
           , "grib_url" .= gribUrl
           ]

instance FromJSON SimForm where
  parseJSON = withObject "SimForm" $ \v -> SimForm
    <$> v .: "lat"
    <*> v .: "lon"
    <*> v .: "alt"
    <*> v .: "time"
    <*> v .: "balloon_mass"
    <*> v .: "ascent_rate"
    <*> v .: "burst_volume"
    <*> v .: "balloon_volume"
    <*> v .: "balloon_coeff_drag"
    <*> v .: "parachute_coeff_drag"
    <*> v .: "parachute_area"
    <*> v .: "package_coeff_drag"
    <*> v .: "grib_url"
