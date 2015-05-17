{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Main where

import System.Environment
import Data.Monoid
import Control.Monad
import Data.Aeson
import Network.HTTP.Conduit
import GHC.Generics
import qualified Data.Text    as T
import qualified Data.Text.IO as T

type City = String
type Code = String
type Emoji = T.Text

data Weather = Weather {description :: String, icon :: String}

instance FromJSON Weather where
    parseJSON (Object o) = do
        weatherValue <- head <$> o .: "weather"
        Weather <$> weatherValue .: "description" <*> weatherValue .: "icon"
    parseJSON _ = mzero

apiUrl :: String
apiUrl = "http://api.openweathermap.org/data/2.5/weather?q="

requestBuilder :: City -> String
requestBuilder city = apiUrl <> city <> "&units=metric"

getWeather :: City -> IO (Maybe Weather)
getWeather city = do
    rawJson <- simpleHttp $ requestBuilder city
    return (decode rawJson :: Maybe Weather)

getEmoji :: Code -> Emoji
getEmoji code = case code of
  "01d" -> "☀️" -- sun
  "02d" -> "⛅️" -- sun with cloud
  "03d" -> "☁️" -- cloud
  "04d" -> "☁️" -- cloud
  "09d" -> "☔️" -- rain
  "10d" -> "☔️" -- rain
  "11d" -> "⚡️" -- thunder
  "13d" -> "❄️" -- snow
  "50d" -> "♒︎" -- mist
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
    Nothing  -> print "Failed to fetch weather info."
