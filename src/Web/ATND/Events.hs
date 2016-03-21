{-# LANGUAGE OverloadedStrings #-}
-- |
-- Implements the \"events\" section of the ATND JSON API.
--
module Web.ATND.Events
  (
  -- * ATND API Methods
  getEvents,
  -- * Parameter types
  EventId(..),
  User(..),
  OwnerId(..),
  -- * Resut Type
  EventResults(..),
  EventResult(..)
  )
  where

import Debug.Trace(trace)
import Web.ATND (endpointUrl, ApiType(..))

import Data.Text (Text, unpack, pack)

import Data.Aeson (decode, Value(..), object, (.=), FromJSON(..), (.:), Value(..))
import Data.Aeson.Types ()
import Data.Aeson.TH ()
import Data.Aeson.Encode ()

import Text.Parsec ()
import Text.Parsec.Text ()
import Control.Exception.Base ()
import Control.Exception.Lifted ()
import Control.Monad (mzero)
import Control.Monad.IO.Class (liftIO)
import Data.Default ()
import qualified Data.ByteString.Char8 as B8
--import qualified Data.ByteString.Lazy as BL
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString.Lazy()
import Data.Typeable ()
import Network.HTTP.Conduit (setQueryString, tlsManagerSettings, parseUrl, newManager, httpLbs, RequestBody(..), Response(..), Request(..))
import Network.HTTP.Types ()
import Network.HTTP.Types.Header ()
import Control.Monad.Reader ()
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger ()
import Control.Monad.Trans.Control ()

-- | Represents an event id
newtype EventId = EventId { unEventId :: Integer }
                  deriving (Show, Eq)

-- | Represents an user id
data User = User { userId :: Text,
          nickname :: Text,
          twitterId :: Text
          }
                  deriving (Show, Eq)

-- | Represents an owner
newtype OwnerId = OwnerId {unOwnerId :: Text}
                  deriving (Show, Eq)

-- | The results of search event
newtype EventResults = EventResults { events :: [EventResult] }
                 deriving (Show, Eq)

-- | The result of search event
data EventResult = EventResult { eventId :: EventId, title :: Text }
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
         -> Maybe [User]
         -- ^ User's id
         -> Maybe [User]
         -- ^ User's nickname
         -> Maybe [User]
         -- ^ User's twitter id
         -> Maybe [Text]
         -- ^ Owner's id
         -> Maybe [Text]
         -- ^ Owner's nickname
         -> Maybe [Text]
         -- ^ Owner's twitter id
         -> Maybe Integer
         -- ^ Start position of results(default: 1)
         -> Maybe Integer
         -- ^ Count of results(default: 10, min: 1, max: 10)
         -> IO EventResults
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
         ownerTwitterIDs
         start
         count = runResourceT $ do
       initReq <- parseUrl $ unpack $ endpointUrl Search
       let request = object [ "event_id" .= fmap (map unEventId) eventIds
                          , "keyword" .= keywords
                          , "keyword_or" .= keywordOrs
                          , "ym" .= yms
                          , "ymd" .= ymds
                          , "user_id" .= fmap (map userId) userIds
                          , "nickname" .= fmap (map nickname) userNicknames
                          , "twitter_id" .= fmap (map twitterId)userTwitterIds
                          , "owner_id" .= ownerIds
                          , "owner_nickname" .= ownerNicknames
                          , "owner_twitter_id" .= ownerTwitterIDs
                          , "start" .= start
                          , "count" .= count
                          , "format" .= ("json"::Text)
                          ]
       let mkq k xs = case xs of
                        Just xb -> map (\x -> ((B8.pack k), Just $ encodeUtf8 x)) xb
                        Nothing -> []
       let query = concat [mkq "keyword" keywords
                         , mkq "keyword_or" keywordOrs  
                         , mkq "keyword_or" keywordOrs 
                         , mkq "ym" yms
                         , mkq "ymd" ymds
                         , mkq "user_id" $ fmap (map userId) userIds 
                         , mkq "nickname" $ fmap (map nickname) userNicknames 
                         , mkq "twitter_id" $ fmap (map twitterId) userTwitterIds 
                         , mkq "owner_id" ownerIds
                         , mkq "owner_nickname" ownerNicknames
                         , mkq "owner_twitter_id" ownerTwitterIDs
                         , mkq "start" $ fmap (\x -> [pack $ show x]) start 
                         , mkq "count" $ fmap (\x -> [pack $ show x]) count
                         , mkq "format" $ Just [pack $ "json"] 
                         ]
       man <- liftIO $ newManager tlsManagerSettings
--       let req = initReq { requestBody = RequestBodyLBS $ encode request }
       let req = initReq
       let req' = setQueryString query req      
       response <- httpLbs req' man
       let jsResult = decode $ responseBody response
       case jsResult of
         Just eventResults -> return eventResults
         Nothing -> liftIO $ mzero

instance FromJSON EventResults where
    parseJSON (Object v) = EventResults <$> v .: "events"
    parseJSON _ = mzero

instance FromJSON EventResult where
    parseJSON (Object v) = EventResult <$>
                           v .: "event" <*>
                           (e >>= (.: "title"))
                           where e = (v .: "event")
    parseJSON _ = mzero

instance FromJSON EventId where
    parseJSON (Object v) = EventId <$> v .: "event_id"
    parseJSON _ = mzero
