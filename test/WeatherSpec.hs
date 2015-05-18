{-# LANGUAGE OverloadedStrings #-}

module WeatherSpec (spec) where

import Weather
import Test.Hspec

spec :: Spec
spec = do
    describe "getEmoji" $ do
        it "returns ⁉ on undefined" $ do
            getEmoji "lol" `shouldBe` "\8265\65039"

        it "returns ⁉ if code length is < 2" $ do
            getEmoji "1" `shouldBe` "\8265\65039"

        it "returns an emoji for valid codes" $ do
            getEmoji "01" `shouldBe` "\9728\65039"
            getEmoji "02" `shouldBe` "\9925\65039"
            getEmoji "03" `shouldBe` "\9729\65039"
            getEmoji "04" `shouldBe` "\9729\65039"
            getEmoji "09" `shouldBe` "\128166"
            getEmoji "10" `shouldBe` "\128166"
            getEmoji "11" `shouldBe` "\9889\65039"
            getEmoji "13" `shouldBe` "\10052\65039"
            getEmoji "50" `shouldBe` "\9810\65038"

    describe "urlBuilder" $ do
      it "return an URL" $ do
          urlBuilder "london" `shouldBe` "http://api.openweathermap.org/data/2.5/weather?q=london&units=metric"
