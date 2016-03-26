{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Implements the \"events\" section of the ATND JSON API.
--
module Web.ATND.Events
  (
  -- * ATND API Methods
  getEvents,
  -- * Parameter types
  EventId(..),
  Person(..),
  -- * Resut Type
  EventResults(..),
  EventResult(..),
  -- * run
  runATND,
  defaultATNDConfig
  )
  where

import Control.Exception.Lifted as CEL
import Debug.Trace(trace)
import Web.ATND (endpointUrl, ApiType(..))

import Data.Text (Text, unpack, pack)
import Data.Typeable

import Data.Aeson (decode, Value(..), object, (.=), ToJSON(..), FromJSON(..), (.:), (.:?), Value(..))
import Data.Aeson.Types ()
import Data.Aeson.TH ()
import Data.Aeson.Encode ()

import Text.Parsec ()
import Text.Parsec.Text ()
import Control.Exception.Base ()
import Control.Exception.Lifted ()
import Control.Monad (mzero, liftM)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Default ()
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString.Lazy()
import Data.Typeable ()
import Network.HTTP.Conduit (HttpException(..), setQueryString, tlsManagerSettings, parseUrl, newManager, httpLbs, RequestBody(..), Response(..), Request(..), Manager)
import Network.HTTP.Types ()
import Network.HTTP.Types.Header ()
import Control.Monad.Reader (runReaderT, ReaderT)
import Control.Monad.Trans.Resource (runResourceT, ResourceT)
import Control.Monad.Logger ()
import Control.Monad.Trans.Control ()

import Data.Time

-- | Represents an event id
newtype EventId = EventId { unEventId :: Integer }
                  deriving (Show, Eq)

-- | Represents an person(user or owner)
data Person = Person { personId :: Integer,
            personNickname :: Text,
            personTwitterId :: Maybe Text
            } deriving (Show, Eq)

-- | Respresents ATND time format
newtype ATNDTime = ATNDTime { unATNDTime :: UTCTime }

instance Eq ATNDTime where
    x == y = unATNDTime x == unATNDTime y

instance Show ATNDTime where
    show x = show $ unATNDTime x

atndTimeFormat :: String
atndTimeFormat = "%FT%T%Q%z"

-- | The results of search event
newtype EventResults = EventResults { events :: [EventResult] }
                 deriving (Show, Eq)

-- | The result of search event
data EventResult = EventResult { 
                 eventId :: EventId, 
                 title :: Text,
                 catch :: Maybe Text,
                 description :: Text,
                 eventUrl :: Text,
                 startedAt :: ATNDTime,
                 endedAt :: ATNDTime,
                 url :: Maybe Text,
                 limit :: Integer,
                 address :: Text,
                 place :: Text,
                 lat :: Text,
                 lon :: Text,
                 owner :: Person,
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
         -> Maybe [Person]
         -- ^ User's id
         -> Maybe [Person]
         -- ^ User's nickname
         -> Maybe [Person]
         -- ^ User's twitter id
         -> Maybe [Person]
         -- ^ Owner's id
         -> Maybe [Person]
         -- ^ Owner's nickname
         -> Maybe [Person]
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
         ownerTwitterIds
         start
         count = runResourceT $ do
       initReq <- parseUrl $ unpack $ endpointUrl Search
       let request = object [ "event_id" .= fmap (map unEventId) eventIds
                          , "keyword" .= keywords
                          , "keyword_or" .= keywordOrs
                          , "ym" .= yms
                          , "ymd" .= ymds
                          , "user_id" .= fmap (map personId) userIds
                          , "nickname" .= fmap (map personNickname) userNicknames
                          , "twitter_id" .= fmap (map (\user -> case personTwitterId user of
                                                 Just uti -> uti 
                                                 Nothing -> "")) userTwitterIds
                          , "owner_id" .= fmap (map personId) ownerIds
                          , "owner_nickname" .= fmap (map personNickname) ownerNicknames
                          , "owner_twitter_id" .= fmap (map (\owner -> case personTwitterId owner of
                                                       Just oti -> oti 
                                                       Nothing -> "")) ownerTwitterIds
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
                         , mkq "user_id" $ fmap (map (pack . show . personId)) userIds 
                         , mkq "nickname" $ fmap (map personNickname) userNicknames 
                         , mkq "twitter_id" $ fmap (map (\user -> case personTwitterId user of
                                                   Just uti -> uti 
                                                   Nothing -> "")) userTwitterIds 
                         , mkq "owner_id" $ fmap (map (pack . show. personId)) ownerIds
                         , mkq "owner_nickname" $ fmap (map personNickname) ownerNicknames
                         , mkq "owner_twitter_id" $ fmap (map (\owner -> case personTwitterId owner of
                                                         Just oti -> oti 
                                                         Nothing -> "")) ownerTwitterIds
                         , mkq "start" $ fmap (\x -> [pack $ show x]) start 
                         , mkq "count" $ fmap (\x -> [pack $ show x]) count
                         , mkq "format" $ Just [pack $ "json"] 
                         ]
--       let req = initReq { requestBody = RequestBodyLBS $ encode request }
       let req = initReq
       let req' = setQueryString query req      
       man <- liftIO $ newManager tlsManagerSettings
       response <- CEL.catch (httpLbs req' man)
         (\e ->
           case e :: HttpException of
             StatusCodeException _ headers _ -> do
               let (mResponse :: Maybe ATNDError) = BL.fromStrict `fmap`
                     (lookup "X-Response-Body-Start" headers) >>= decode
               maybe (throwIO e) id (throwIO `fmap` mResponse)
             _ -> throwIO e)
       let jsResult = decode $ responseBody response
       case jsResult of
         Just eventResults -> return eventResults
         Nothing -> throwIO $ OtherATNDError (-1) "Parse Error: Could not parse result JSON from ATND"

instance ToJSON EventId where
    toJSON (EventId t) = object ["event_id" .= t]

instance FromJSON EventResults where
    parseJSON (Object v) = EventResults <$> v .: "events"
    parseJSON _ = mzero

parseATNDTime :: String -> Maybe UTCTime
parseATNDTime = parseTimeM True defaultTimeLocale "%FT%T%Q%z"

instance FromJSON ATNDTime where
    parseJSON (String s) = maybe mzero (return . ATNDTime) $ 
      parseTimeM True defaultTimeLocale atndTimeFormat (unpack s) 
    parseJSON _ = mzero

instance FromJSON EventResult where
    parseJSON (Object v) = EventResult <$>
                           v .: "event" <*>
                           (e >>= (.: "title")) <*>
                           (e >>= (.:? "catch")) <*>
                           (e >>= (.: "description")) <*>
                           (e >>= (.: "event_url")) <*>
                           (e >>= (.: "started_at")) <*>
                           (e >>= (.: "ended_at")) <*>
                           (e >>= (.:? "url")) <*>
                           (e >>= (.: "limit")) <*>
                           (e >>= (.: "address")) <*>
                           (e >>= (.: "place")) <*>
                           (e >>= (.: "lat")) <*>
                           (e >>= (.: "lon")) <*>
                           (Person <$>
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

data ATNDError = NotFoundError Int Text
               | OtherATNDError Int Text
               deriving (Typeable, Show, Eq)

instance Exception ATNDError

instance FromJSON ATNDError where
    parseJSON (Object v) = do
      status <- v .: "status"
      message <- v .: "error"
      return $ (errConstructor status) ((read $ unpack status)::Int) message
     where
       errConstructor status = case (status :: Text) of
                                 "404" -> NotFoundError
                                 _ -> OtherATNDError
    parseJSON _ = mzero

data ATNDConfig = ATNDConfig { atndManager :: Manager }

-- | create a ATNDConfig with a new Manager
defaultATNDConfig :: MonadIO m => m ATNDConfig
defaultATNDConfig = do
      man <- liftIO $ newManager tlsManagerSettings
      return ATNDConfig { atndManager = man }

type ATND a = ReaderT ATNDConfig (ResourceT IO) a

runATND :: (MonadIO m) => ATNDConfig -> ATND a -> m a
runATND config action =
    liftIO $ runResourceT $ runReaderT action config
