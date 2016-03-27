{-# LANGUAGE OverloadedStrings #-}
module Web.ATNDSpec(main, spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception(evaluate)

import Web.ATND 
import Web.ATND.Event
import Web.ATND.Util
import Data.Text(Text)
import Control.Monad.Logger
import Control.Monad.IO.Class

main :: IO ()
main = hspec spec 

keywds :: Maybe [Text]
keywds = Just ["haskell"]

spec :: Spec
spec = do 
  describe "Web.ATND.runATNDT" $ do
    it "returns the list of events" $ do
      cfg <- defaultATNDConfig
      res <- liftIO $ runNoLoggingT $ runATNDT cfg $ getEvents Nothing keywds 
        Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing 
      (length $ events res) `shouldSatisfy` (>0)

  describe "Web.ATND.runATND" $ do
    it "returns the list of events" $ do
      cfg <- defaultATNDConfig
      res <- runATND cfg $ getEvents Nothing keywds 
        Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing 
      (length $ events res) `shouldSatisfy` (>0)
