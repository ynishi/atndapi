{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.ATND() 
import Web.ATND.Events(getEvents, title, eventId, events)
import Data.Text(Text)

main :: IO ()
main = do
    res <- getEvents Nothing (Just (["python"::Text])) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing 
    putStrLn $ show $ map (\x -> (show $ title x) ++ (show $ eventId x)) $ events res
