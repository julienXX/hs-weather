{-# LANGUAGE OverloadedStrings #-}

module Weather where

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
  then "\8265\65039"           -- Unknown
  else case x of
        "01" -> "\9728\65039"  -- Sun
        "02" -> "\9925\65039"  -- Sun with cloud
        "03" -> "\9729\65039"  -- Cloud
        "04" -> "\9729\65039"  -- Cloud
        "09" -> "\128166"      -- Rain
        "10" -> "\128166"      -- Rain
        "11" -> "\9889\65039"  -- Thunder
        "13" -> "\10052\65039" -- Snow
        "50" -> "\9810\65038"  -- Mist
        _ -> "\8265\65039"     -- Unknown
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
