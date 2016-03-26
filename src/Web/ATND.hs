{-# LANGUAGE OverloadedStrings #-}
module Web.ATND 
    (
      endpointUrl,
      ApiType(..)
    ) where

import Data.Text(Text)
import qualified Data.Text as D (concat)

-- | Builds the ATND endpoint URL
-- make https://api.atnd.org/events/[usres]
endpointUrl :: ApiType -> Text
endpointUrl a = D.concat ["https://api.atnd.org/", apiType a] 

-- | type of ATND api
data ApiType = Users | Search deriving(Show, Eq)

-- | get ATND api type text
apiType :: ApiType -> Text
apiType Users = "events/users"
apiType Search = "events"
