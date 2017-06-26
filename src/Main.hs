{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (when)
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.HABSim.Grib2.CSVParse.Types
import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import Lucid
import Lucid.Base
import Network.HTTP.Client
import qualified Shelly as S
import System.Directory (removeFile)
import System.IO.Temp (emptySystemTempFile)
import Text.Digestive.Scotty
import Text.Digestive.View
import Web.Scotty

-- Our modules
import Form
import HTML
import SimResult
import Types

getGrib :: T.Text -> IO BL.ByteString
getGrib url = do
  manager <- newManager defaultManagerSettings
  request <- parseRequest (T.unpack url)
  response <- httpLbs request manager
  tmp <- emptySystemTempFile "grib"
  tmpcsv <- emptySystemTempFile "grib.csv"
  putStrLn tmp
  putStrLn tmpcsv
  BL.writeFile tmp (responseBody response)
  putStrLn "shelly"
  csv <- S.shelly . S.silently $
    S.run "wgrib2" [T.pack tmp, "-csv", T.pack tmpcsv]
  putStrLn "back"
  out <- BL.readFile tmpcsv
  putStrLn "readfile"
  removeFile tmp
  removeFile tmpcsv
  putStrLn "ret"
  return out

doSim :: SimForm -> ActionM ()
doSim sf = do
  grib <- liftIO $ getGrib (fGribUrl sf)
  let gribLines = getGribLines grib
      simRes = runSim gribLines sf
  html . renderText $ template "HABSim: Simulation Result" $
    renderSim simRes

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
      Just sf -> doSim sf
      _ -> formTemplate view
