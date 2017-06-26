module Types where
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
