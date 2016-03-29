{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Implements the \"events\" section of the ATND JSON API.
--
module Web.ATND.Event
  (
  -- * API Method 
  getEvents,
  -- * Parameter type
  EventId(..),
  -- * Result type
  Events(..),
  Event(..),
  )
  where

import Web.ATND
import Web.ATND.Util

import Data.Text (Text, pack)
import Data.Aeson (Value(..), object, (.=), ToJSON(..), FromJSON(..), (.:), (.:?), Value(..))
import Control.Monad (mzero)

-- | Represent an event id
newtype EventId = EventId { unEventId :: Integer }
                  deriving (Show, Eq)

-- | Represent events 
newtype Events = Events { events :: [Event] }
                 deriving (Show, Eq)

-- | Represent an event 
data Event = Event { 
           eventId :: EventId, 
           title :: Text,
           catch :: Maybe Text,
           description :: Text,
           eventUrl :: Text,
           startedAt :: ATNDTime,
           endedAt :: ATNDTime,
           url :: Maybe Text,
           limit :: Maybe Integer,
           address :: Text,
           place :: Text,
           lat :: Maybe Text,
           lon :: Maybe Text,
           owner :: Web.ATND.Util.Person,
           accepted :: Integer,
           waiting :: Integer,
           updatedAt :: ATNDTime 
           }
           deriving (Show, Eq)

-- | Get event data(JSON) 
getEvents :: Maybe [EventId]
         -- ^ Event's id
         -> Maybe [Text]
         -- ^ List of keywords(and)
         -> Maybe [Text]
         -- ^ List of keywords(or)
         -> Maybe [Text]
         -- ^ hold on year-month(yyyymm)
         -> Maybe [Text]
         -- ^ hold on date(yyyymmdd)
         -> Maybe [Web.ATND.Util.Person]
         -- ^ User's id
         -> Maybe [Web.ATND.Util.Person]
         -- ^ User's nickname
         -> Maybe [Web.ATND.Util.Person]
         -- ^ User's twitter id
         -> Maybe [Web.ATND.Util.Person]
         -- ^ Owner's id
         -> Maybe [Web.ATND.Util.Person]
         -- ^ Owner's nickname
         -> Maybe [Web.ATND.Util.Person]
         -- ^ Owner's twitter id
         -> Maybe Integer
         -- ^ Start position of results(default: 1)
         -> Maybe Integer
         -- ^ Count of results(default: 10, min: 1, max: 10)
         -> ATNDT m Events
getEvents eventIds
         keywords
         keywordOrs
         yms
         ymds
         userIds
         userNicknames
         userTwitterIds
         ownerIds
         ownerNicknames
         ownerTwitterIds
         start
         count = do
       query EventApi queryList 
       where
         queryList = toQueryList [("events", fmap (map (pack . show . unEventId)) eventIds)
                                , ("keyword", keywords)
                                , ("keyword_or", keywordOrs)
                                , ("ym", yms)
                                , ("ymd", ymds)
                                , ("user_id", fmap (map (pack . show . personId)) userIds)
                                , ("nickname", fmap (map personNickname) userNicknames)
                                , ("twitter_id", fmap (map (\user -> case personTwitterId user of
                                    Just uti -> uti 
                                    Nothing -> "")) userTwitterIds)
                                , ("owner_id", fmap (map (pack . show. personId)) ownerIds)
                                , ("owner_nickname", fmap (map personNickname) ownerNicknames)
                                , ("owner_twitter_id", fmap (map (\o -> case personTwitterId o of
                                    Just oti -> oti 
                                    Nothing -> "")) ownerTwitterIds)
                                , ("start", fmap (\x -> [pack $ show x]) start)
                                , ("count", fmap (\x -> [pack $ show x]) count)
                                , ("format", Just [pack $ "json"])
                                ]

instance ToJSON EventId where
    toJSON (EventId t) = object ["event_id" .= t]

instance FromJSON Events where
    parseJSON (Object v) = Events <$> v .: "events"
    parseJSON _ = mzero

instance FromJSON Event where
    parseJSON (Object v) = Event <$>
                           v .: "event" <*>
                           (e >>= (.: "title")) <*>
                           (e >>= (.:? "catch")) <*>
                           (e >>= (.: "description")) <*>
                           (e >>= (.: "event_url")) <*>
                           (e >>= (.: "started_at")) <*>
                           (e >>= (.: "ended_at")) <*>
                           (e >>= (.:? "url")) <*>
                           (e >>= (.:? "limit")) <*>
                           (e >>= (.: "address")) <*>
                           (e >>= (.: "place")) <*>
                           (e >>= (.:? "lat")) <*>
                           (e >>= (.:? "lon")) <*>
                           (Web.ATND.Util.Person <$>
                             (e >>= (.: "owner_id")) <*>
                             (e >>= (.: "owner_nickname")) <*>
                             (e >>= (.:? "owner_twitter_id"))) <*>
                           (e >>= (.: "accepted")) <*>
                           (e >>= (.: "waiting")) <*>
                           (e >>= (.: "updated_at"))
                           where e = (v .: "event")
    parseJSON _ = mzero

instance FromJSON EventId where
    parseJSON (Object v) = EventId <$> v .: "event_id"
    parseJSON _ = mzero
