{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.ATND 
import Web.ATND.Event
import Web.ATND.Util
import Data.Text(Text)

main :: IO ()
main = do
    cfg <- defaultATNDConfig
    res <- runATND cfg $ getEvents Nothing (Just ["python"::Text]) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing 
    -- putStrLn $ show $ map (\x -> "ti: " ++ (show $ title x) ++ " id: " ++ (show $ eventId x)) $ events res
    print $ show res 
