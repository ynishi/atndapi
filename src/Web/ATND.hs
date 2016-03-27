{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
--
-- Module for interface of the ATND JSON API.
--
-- Example usage to get events (in IO monad):
-- 
-- > cfg <- defaultATNDConfig
-- > runATND cfg $ do
-- >   getEvents <[eventId]> <[keywords]> ... Nothing ...
-- > 
-- 
-- To run in a monad that implements MonadIO, MonadLogger and
-- MonadBaseControl IO (such as a Yesod Handler), use ATNDM.
--
module Web.ATND 
    (
      -- * The ATND/ATNDT monad 
      ATND,
      ATNDT,
      runATND,
      runATNDT,
      -- * Running query
      query,
      query',
      -- * Config
      ATNDConfig(..),
      defaultATNDConfig,
      -- * Error Handling
      ATNDError 
    ) where

import Web.ATND.Util

import Data.Text(Text, unpack, pack)
import qualified Data.Text as D (concat)
import Data.Typeable

import Data.Aeson (decode, Value(..), FromJSON(..), (.:))

import Control.Monad (mzero)
import Control.Monad.IO.Class (liftIO, MonadIO)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Conduit (HttpException(..), setQueryString, tlsManagerSettings, parseUrl, newManager, httpLbs, Response(..), Request(..), Manager)
import Network.HTTP.Types.Header (ResponseHeaders)
import Control.Monad.Reader (runReaderT, ReaderT)
import Control.Monad.Logger
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Exception.Lifted as CEL

-- | Represent the config for ATND/ATNDT
data ATNDConfig = ATNDConfig { atndManager :: Manager }

-- | Create a ATNDConfig with a new Manager
defaultATNDConfig :: MonadIO m => m ATNDConfig
defaultATNDConfig = do
      man <- liftIO $ newManager tlsManagerSettings
      return ATNDConfig { atndManager = man }

-- | ATND monad transformer
type ATNDT m a = (MonadIO m, MonadLogger m, MonadBaseControl IO m) => ReaderT ATNDConfig m a

-- | Run ATNDT
runATNDT :: (MonadIO m, MonadLogger m, MonadBaseControl IO m) => ATNDConfig -> ATNDT m a -> m a
runATNDT config action =
    runReaderT action config

-- | Alias of ATNDT with Logging 
type ATND a = ATNDT(LoggingT IO) a 

-- | Run ATND in IO, ignoring the existing monadic context and logging to stderr.
runATND :: (MonadIO m) => 
  (ATNDConfig -> ATNDT(LoggingT IO) a -> m a)
runATND config action =
    liftIO $ runStderrLoggingT $ runATNDT config action

-- | Build the ATND endpoint URL
-- make https://api.atnd.org/events/[usres]
endpointUrl :: Section -> Text
endpointUrl a = D.concat ["https://api.atnd.org/", fromSection a] 

-- | Run a query to ATND. Remove Nothing in the query list. 
query :: (FromJSON x) => Section 
       -- ^ ATND api type
       -> [(B8.ByteString, Maybe B8.ByteString)] 
       -- ^ List for Query String  
       -> ATNDT m x
query section queryList = do
       query' section $ filterQuery queryList 

-- | Run a query to ATND, for apply query list directry.
query' :: (FromJSON x) => Section -> [(B8.ByteString, Maybe B8.ByteString)] -> ATNDT m x
query' section queryList = do 
       config <- defaultATNDConfig 
       initReq <- liftIO $ parseUrl $ unpack $ endpointUrl section 
       let req = setQueryString queryList  $ initReq
       $(logDebug) $ pack . show $ queryString req 
       response <- CEL.catch (httpLbs req $ atndManager config) catchHttpException
       $(logDebug) $ pack . show $ responseBody response 
       case decode $ responseBody response of
         Just eventResults -> return eventResults
         Nothing -> throwIO $ OtherATNDError (-1) "Parse Error: Could not parse result JSON from ATND"
       where
         catchHttpException :: HttpException -> ATNDT m a
         catchHttpException e@(StatusCodeException _ headers _) = do
           $(logDebug) $ pack . show $ lookup "X-Response-Boby-Start" headers 
           maybe (throwIO e) throwIO (decodeError headers)
         catchHttpException e = throwIO e
         decodeError :: ResponseHeaders -> Maybe ATNDError
         decodeError headers = BL.fromStrict `fmap` lookup "X-Response-Body-Start" headers >>= decode

-- | Error of ATND API
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
