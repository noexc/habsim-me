{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (when, void)
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.HABSim.Grib2.CSVParse.Types
import qualified Data.HashMap.Lazy as HM
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Database.Redis as R
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
  BL.writeFile tmp (responseBody response)
  csv <- S.shelly . S.silently $
    S.run "wgrib2" [T.pack tmp, "-csv", T.pack tmpcsv]
  out <- BL.readFile tmpcsv
  removeFile tmp
  removeFile tmpcsv
  return out

doSim :: SimForm -> IO B.ByteString
doSim sf = do
  grib <- liftIO $ getGrib (fGribUrl sf)
  conn <- R.checkedConnect $ R.defaultConnectInfo {
    R.connectHost = "habsim-redis"
  }
  let gribLines = getGribLines grib
      simRes = runSim gribLines sf
  uuid <- UUID.toASCIIBytes <$> UUID.nextRandom
  R.runRedis conn $ do
    -- TODO: Do something if setnx -> false
    R.setnx uuid (TE.encodeUtf8 simRes)
    R.setnx (uuid <> ".input") (BL.toStrict . encode $ sf)
    R.quit
  return uuid

recallSim :: B.ByteString -> ActionM ()
recallSim uuid = do
  conn <- liftIO . R.checkedConnect $ R.defaultConnectInfo {
    R.connectHost = "habsim-redis"
  }
  simMay <- liftIO . R.runRedis conn $ do
    simRes <- R.get uuid
    simInput <- R.get (uuid <> ".input")
    R.quit
    return (simRes, simInput)
  case simMay of
    (Left _, _) -> raise "Error while talking to redis"
    (Right (Just simRes), simInputs) ->
      html . renderText $ template "HABSim: Simulation Result" $
        renderSim (TE.decodeUtf8 simRes) (decodeDontCare simInputs)
    (Right Nothing, _) -> raise "UUID Not found"

decodeDontCare :: Either R.Reply (Maybe B.ByteString) -> Maybe SimForm
decodeDontCare (Left _) = Nothing
decodeDontCare (Right Nothing) = Nothing
decodeDontCare (Right (Just s)) = decode . BL.fromStrict $ s

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
      Just sf -> do
        x <- liftIO $ doSim sf
        void . redirect . TL.fromStrict . TE.decodeUtf8 $ x
      _ -> formTemplate view
  get "/:uuid" $ do
    uuid <- param "uuid"
    recallSim uuid
