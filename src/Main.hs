{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (putStrLn)

import System.Environment (getArgs)
import Data.Maybe (listToMaybe)
import Data.Monoid ((<>))
import Data.Aeson (FromJSON(..), (.:), decode, withObject)
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
  parseJSON = withObject "Weather" $ \o -> do
    weatherValue <- head <$> o .: "weather"
    Weather <$> weatherValue .: "icon"

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
getWeather city =  decode <$> (httpRequest $ urlBuilder city)

getEmoji :: Code -> Emoji
getEmoji weather_code =
  if length weather_code < 2
  then "⁉️"
  else case x of
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
       where x = take 2 weather_code

parseArgs :: IO City
parseArgs = readCity <$> getArgs
  where readCity args = case listToMaybe args of
                         Nothing -> error "No City given."
                         Just s -> s

main :: IO ()
main = do
   city <- parseArgs
   response <- getWeather city
   case response of
     (Just w) -> putStrLn $ getEmoji $ code $ w
     Nothing  -> error "Failed to fetch weather info."
