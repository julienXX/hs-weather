{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (putStrLn)

import System.Environment (getArgs)
import Data.Monoid ((<>))
import Control.Monad (mzero)
import Data.Aeson (FromJSON(..), (.:), decode, Value(..))
import Network.HTTP.Client (parseUrl, withManager, defaultManagerSettings, httpLbs, responseBody)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Text.IO (putStrLn)

type URL = String
type City = String
type Code = String
type Emoji = Text

data Weather = Weather { code :: String }

instance FromJSON Weather where
  parseJSON (Object o) = do
    weatherValue <- head <$> o .: "weather"
    Weather <$> weatherValue .: "icon"
  parseJSON _ = mzero

apiUrl :: URL
apiUrl = "http://api.openweathermap.org/data/2.5/weather?q="

urlBuilder :: City -> URL
urlBuilder city = apiUrl <> city <> "&units=metric"

httpRequest :: URL -> IO ByteString
httpRequest str = do
  req <- parseUrl str
  withManager defaultManagerSettings $ \mgr -> do
     resp <- httpLbs req mgr
     return (responseBody resp)

getWeather :: City -> IO (Maybe Weather)
getWeather city = do
  rawJson <- httpRequest $ urlBuilder city
  return (decode rawJson :: Maybe Weather)

getEmoji :: Code -> Emoji
getEmoji code = case take 2 code of
  "01" -> "☀️" -- sun
  "02" -> "⛅️" -- sun with cloud
  "03" -> "☁️" -- cloud
  "04" -> "☁️" -- cloud
  "09" -> "💦" -- rain
  "10" -> "💦" -- rain
  "11" -> "⚡️" -- thunder
  "13" -> "❄️" -- snow
  "50" -> "♒︎" -- mist
  _ -> "⁉️"

parseArgs = do
  args <- getArgs
  return $ case args of
            [] -> error "No City given."
            [s]-> s

main = do
  city <- parseArgs
  response <- getWeather city
  case response of
    (Just w) -> putStrLn $ getEmoji $ code $ w
    Nothing  -> error "Failed to fetch weather info."
