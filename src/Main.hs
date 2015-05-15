{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment
import Data.Monoid
import Data.Aeson
import Network.HTTP.Conduit

type City = String

apiUrl :: String
apiUrl = "http://api.openweathermap.org/data/2.5/weather?q="

requestBuilder :: City -> String
requestBuilder city = apiUrl <> city <> "&units=metric"

getWeather :: City -> IO (Maybe Value)
getWeather city = do
    rawJson <- simpleHttp $ requestBuilder city
    return (decode rawJson :: Maybe Value)

main = do
  [city] <- getArgs
  response <- getWeather city
  case response of
    (Just w) -> print . show $ w
    Nothing  -> print "Failed to fetch weather info."
