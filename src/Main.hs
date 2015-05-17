{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Main where

import System.Environment
import Data.Monoid
import Control.Monad
import Data.Aeson
import Network.HTTP.Conduit
import GHC.Generics

type City = String
type Code = String
type Emoji = String

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

-- getEmoji :: Code -> Emoji
-- getEmoji code = case code of
--   "01d" = "sun"
--   "02d" = "sun with cloud"
--   "03d" = "cloud"
--   "04d" = "bad cloud"
--   "09d" = "bad rain"
--   "10d" = "sun rain"
--   "11d" = "thunder"
--   "13d" = "snow"
--   "50d" = "mist"

parseArgs = do
    args <- getArgs
    return $ case args of
                  [] -> error "No City given."
                  [s]-> s

main = do
  city <- parseArgs
  response <- getWeather city
  case response of
    (Just w) -> print . show $ w
    Nothing  -> print "Failed to fetch weather info."
