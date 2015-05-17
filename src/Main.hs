{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment
import Data.Monoid
import Control.Monad
import Data.Aeson
import Network.HTTP.Conduit
import qualified Data.Text    as T
import qualified Data.Text.IO as T

type URL = String
type City = String
type Code = String
type Emoji = T.Text

data Weather = Weather { icon :: String }

instance FromJSON Weather where
    parseJSON (Object o) = do
        weatherValue <- head <$> o .: "weather"
        Weather <$> weatherValue .: "icon"
    parseJSON _ = mzero

apiUrl :: URL
apiUrl = "http://api.openweathermap.org/data/2.5/weather?q="

requestBuilder :: City -> URL
requestBuilder city = apiUrl <> city <> "&units=metric"

getWeather :: City -> IO (Maybe Weather)
getWeather city = do
    rawJson <- simpleHttp $ requestBuilder city
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
    (Just w) -> T.putStrLn $ getEmoji $ icon $ w
    Nothing  -> error "Failed to fetch weather info."
