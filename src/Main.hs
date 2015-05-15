{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Main where

import System.Environment
import Data.Monoid
import Data.Aeson
import Network.HTTP.Conduit
import GHC.Generics

type City = String
type Code = String
type Emoji = String

data Weather =
  Weather { weather :: !String
          , cod :: !Int } deriving (Show, Generic)

instance FromJSON Weather
instance ToJSON Weather

data WeatherDetail =
  WeatherDetail { icon :: !String } deriving (Show, Generic)

instance FromJSON WeatherDetail
instance ToJSON WeatherDetail

apiUrl :: String
apiUrl = "http://api.openweathermap.org/data/2.5/weather?q="

requestBuilder :: City -> String
requestBuilder city = apiUrl <> city <> "&units=metric"

getWeather :: City -> IO (Maybe Value)
getWeather city = do
    rawJson <- simpleHttp $ requestBuilder city
    return (decode rawJson :: Maybe Value)

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
