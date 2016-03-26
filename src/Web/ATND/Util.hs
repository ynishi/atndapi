{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Utility of ATND API method 
--
module Web.ATND.Util
  (
    -- * data 
    Section(..),
    ATNDTime(..),
    Person(..),
    -- * method
    fromSection,
    toQueryList,
    filterQuery 
  ) where

import Data.Aeson (Value(..), FromJSON(..))

import Data.Text (Text, unpack)
import qualified Data.ByteString.Char8 as B8
import Data.Text.Encoding (encodeUtf8)
import Data.Time

import Control.Monad (mzero)


-- | ATND api type
data Section = UserApi | EventApi deriving(Show, Eq)

-- | convert ATND api type to path 
fromSection :: Section -> Text
fromSection UserApi = "events/users"
fromSection EventApi = "events"

-- | Respresents ATND time format
newtype ATNDTime = ATNDTime { unATNDTime :: UTCTime }

instance Eq ATNDTime where
    x == y = unATNDTime x == unATNDTime y

instance Show ATNDTime where
    show x = show $ unATNDTime x

atndTimeFormat :: String
atndTimeFormat = "%FT%T%Q%z"
  
instance FromJSON ATNDTime where
    parseJSON (String s) = maybe mzero (return . ATNDTime) $ 
      parseTimeM True defaultTimeLocale atndTimeFormat (unpack s) 
    parseJSON _ = mzero

-- | Represent an person(user or owner)
data Person = Person { personId :: Integer,
            personNickname :: Text,
            personTwitterId :: Maybe Text
            } deriving (Show, Eq)

-- | Remove Nothing from query list.
filterQuery :: [(a, Maybe a)] -> [(a, Maybe a)]
filterQuery = filter isNothing
       where
         isNothing (_, Nothing) = False
         isNothing _ = True

-- | Make query list 
toQueryList :: [(B8.ByteString, Maybe [Text])] -> [(B8.ByteString, Maybe B8.ByteString)]
toQueryList xs = concat $ map (\(k, ys) -> case ys of
       Just zs -> map (\x -> (k, Just $ encodeUtf8 x)) zs
       Nothing -> []) xs

