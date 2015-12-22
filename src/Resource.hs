{-# LANGUAGE OverloadedStrings #-}

module Resource
( Monitor(..)
, DatadogMonitor(..)
, OldMonitor(..)
, encodePrettyMonitor
, sendToDatadog
, getFromDatadog
, getAllFromDatadog
, isSimilar
, pairSimilar
, isUpdate
, groupToFilePath
, groupToRemote
, groupToForce
) where

import Control.Exception (AssertionFailed(..), assert, throwIO)
import Control.Monad (liftM2)

import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Aeson.Types
import Data.ByteString.Lazy (ByteString)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

import Network.HTTP.Client hiding (path)


data Monitor = Monitor { monitorName :: T.Text
                       , monitorQuery :: T.Text
                       , monitorMessage :: T.Text
                       , monitorNoDataTimeframe :: Integer
                       , monitorTimeoutHours :: Integer
                       , monitorRenotifyInterval :: Integer
                       } deriving (Eq)

instance Show Monitor where
  show m = "\"" ++ T.unpack (monitorName m) ++ "\""

instance ToJSON Monitor where
  toJSON m = object ["name" .= monitorName m
                    ,"query" .= monitorQuery m
                    ,"message" .= monitorMessage m
                    ,"no_data_timeframe" .= if monitorNoDataTimeframe m > 0
                                            then Number (fromIntegral (monitorNoDataTimeframe m))
                                            else Null
                    ,"timeout_hours" .= if monitorTimeoutHours m > 0
                                        then Number (fromIntegral (monitorTimeoutHours m))
                                        else Null
                    ,"renotify_interval" .= if monitorRenotifyInterval m > 0
                                            then Number (fromIntegral (monitorRenotifyInterval m))
                                            else Null
                    ]

instance FromJSON Monitor where
  parseJSON (Object v) = modifyFailure ("Could not decode as a monitor: " ++) $
                         Monitor <$>
                         v .: "name" <*>
                         v .: "query" <*>
                         v .: "message" <*>
                         v .:? "no_data_timeframe" .!= 0 <*>
                         v .:? "timeout_hours" .!= 0 <*>
                         v .:? "renotify_interval" .!= 0
  parseJSON _ = fail "Could not decode as a monitor"

encodePrettyMonitor :: ToJSON a => a -> ByteString
encodePrettyMonitor = encodePretty' (defConfig { confCompare = keyOrder order })
  where order = ["name"
                ,"query"
                ,"message"
                ,"no_data_timeframe"
                ,"timeout_hours"
                ,"renotify_interval"]

newtype DatadogMonitor = DatadogMonitor Monitor

instance ToJSON DatadogMonitor where
  toJSON (DatadogMonitor m) = object ["type" .= (kind :: String)
                                     ,"name" .= monitorName m
                                     ,"query" .= monitorQuery m
                                     ,"message" .= monitorMessage m
                                     ,"options" .= options
                                     ]
    where kind
            | T.isPrefixOf "events(" (monitorQuery m) = "event alert"
            | T.isSuffixOf "count_by_status()" (monitorQuery m) = "service check"
            | otherwise = "metric alert"
          options = object ["timeout_h" .= monitorTimeoutHours m
                           ,"no_data_timeframe" .= max 2 (monitorNoDataTimeframe m)
                           ,"notify_no_data" .= (monitorNoDataTimeframe m > 0)
                           ,"renotify_interval" .= monitorRenotifyInterval m
                           ,"notify_audit" .= False
                           ,"escalation_message" .= ("" :: T.Text)
                           ]

instance FromJSON DatadogMonitor where
  parseJSON (Object v) = modifyFailure ("Could not decode as a monitor: " ++) $
                         DatadogMonitor <$>
                         (Monitor <$>
                          v .:? "name" .!= "" <*>
                          v .: "query" <*>
                          v .:? "message" .!= "" <*>
                          liftM2 (\b n -> if b then n else 0) nnd ndt <*>
                          opt (\w -> w .:? "timeout_h" .!= 0) <*>
                          opt (\w -> w .:? "renotify_interval" .!= 0))
    where opt f = (v .: "options") >>= withObject "" f
          nnd = opt (\w -> w .:? "notify_no_data" .!= False)
          ndt = opt (\w -> w .:? "no_data_timeframe" .!= 0)
  parseJSON _ = fail "Could not decode as a monitor"

newtype OldMonitor = OldMonitor DatadogMonitor

instance FromJSON OldMonitor where
  parseJSON (Object v) = modifyFailure ("Could not decode as a monitor: " ++) $
                         OldMonitor <$>
                         v .: "monitor"
  parseJSON _ = fail "Could not decode as a monitor"

newtype DatadogID = DatadogID Int

instance FromJSON DatadogID where
  parseJSON (Object v) = modifyFailure ("Could not decode as an ID: " ++) $
                         DatadogID <$> v .: "id"
  parseJSON _ = fail "Could not decode as an ID"


getFromDatadog :: Manager -> (String,String) -> Int -> IO (Int,Monitor)
getFromDatadog manager (api,app) i = do
  initReq <- parseUrl ("https://app.datadoghq.com/api/v1/monitor/" ++ show i)
  let apiQuery = [("api_key", api)
                 ,("application_key", app)]
      fullQuery = map (\(a,b) -> (encodeUtf8 (T.pack a), Just (encodeUtf8 (T.pack b)))) apiQuery
      request = setQueryString fullQuery $ initReq { method = "GET" }
  rBody <- responseBody <$> httpLbs request manager
  a <- either (throwIO . AssertionFailed) (\(DatadogID j) -> assert (j == i) return j) (eitherDecode rBody)
  b <- either (throwIO . AssertionFailed) (\(DatadogMonitor m) -> return m) (eitherDecode rBody)
  return (a,b)


getAllFromDatadog :: Manager -> (String,String) -> IO [(Int,Monitor)]
getAllFromDatadog manager (api,app) = do
  initReq <- parseUrl "https://app.datadoghq.com/api/v1/monitor"
  let apiQuery = [("api_key", api)
                 ,("application_key", app)]
      fullQuery = map (\(a,b) -> (encodeUtf8 (T.pack a), Just (encodeUtf8 (T.pack b)))) apiQuery
      request = setQueryString fullQuery $ initReq { method = "GET" }
  rBody <- responseBody <$> httpLbs request manager
  as <- either (throwIO . AssertionFailed) (return . map (\(DatadogID i) -> i)) (eitherDecode rBody)
  bs <- either (throwIO . AssertionFailed) (return . map (\(DatadogMonitor m) -> m)) (eitherDecode rBody)
  return $ zip as bs


sendToDatadog :: Manager -> (String,String) -> Maybe Int -> Monitor -> IO Int
sendToDatadog manager (api,app) mi m = do
  initReq <- parseUrl ("https://app.datadoghq.com/api/v1/monitor" ++ maybe "" (\i -> "/" ++ show i) mi)
  let body = RequestBodyLBS (encode (DatadogMonitor m))
      headers = [("Content-type", "application/json")]
      apiQuery = [("api_key", api)
                 ,("application_key", app)]
      fullQuery = map (\(a,b) -> (encodeUtf8 (T.pack a), Just (encodeUtf8 (T.pack b)))) apiQuery
      request = setQueryString fullQuery $
                initReq { method = maybe "POST" (const "PUT") mi
                        , requestBody = body
                        , requestHeaders = headers
                        }
  rBody <- responseBody <$> httpLbs request manager
  either (throwIO . AssertionFailed) (\(DatadogID i) -> return i) (eitherDecode rBody)


isSimilar :: Monitor -> Monitor -> Bool
-- ^Can a monitor be considered an updated version of another?
-- This differs from sameness in that the two monitors aren't quite the same,
-- but are similar enough that they can be considered versions of each other.
isSimilar a b =
  monitorQuery a == monitorQuery b ||
  monitorName a == monitorName b
  -- TODO: Enable after adding unique descriptions to existing monitorscds
  -- monitorMessage a == monitorMessage b


pairSimilar :: [(a,Monitor)] -> [((a,Monitor),(a,Monitor))]
-- ^Pair-up resources that are similar.
-- Note that this function avoids duplication, i.e. is (a,b) is in the result,
-- (b,a) will not be.
pairSimilar [] = []
pairSimilar (x:xs) = let ys = filter (isSimilar (snd x) . snd) xs
                     in map (\y -> (x,y)) ys ++ pairSimilar xs


isUpdate :: Monitor -> Monitor -> Bool
-- ^Can a resource be considered an update of another?
-- This is the same as two resources being similar, but not the same (there is
-- no need for an update if they are the same)
isUpdate a b = isSimilar a b && a /= b


findUpdate :: Monitor -> [(b,Monitor)] -> Maybe (b,Monitor)
-- ^Find a resource that can be associated with an incumbent resource
findUpdate resource = listToMaybe . filter (isUpdate resource . snd)


groupToFilePath :: FilePath -> [(Int,Monitor)] -> [(FilePath,Monitor)] -> [(FilePath,[(Maybe Int,Monitor)])]
-- ^Group resources by their destination files
-- This function attempts to replace file resources with id resources when
-- possible. When a id resource cannot be used as an update for a file
-- resource, it will be grouped into the default path.
groupToFilePath path incoming outgoing =
  Map.toList $
  foldr (\(fpath,resource) -> Map.alter (Just . maybe [resource] (resource:)) fpath) Map.empty $
  incoming' ++ outgoing'
  where incomingNew = filter (\(_,i) -> all (not . isUpdate i . snd) outgoing && all ((i/=) . snd) outgoing) incoming
        incomingOld = filter (\(_,i) -> any (isUpdate i . snd) outgoing) incoming
        incoming' = map (\(c,i) -> (path,(Just c,i))) incomingNew
        outgoing' = map (\(p,o) -> maybe (p,(Nothing,o)) (\(c,i) -> (p,(Just c,i))) (findUpdate o incomingOld)) outgoing


groupToRemote :: [(FilePath,Monitor)] -> [(Int,Monitor)] -> [(Maybe Int,(FilePath,Monitor))]
-- ^Group filepath monitors by their remote counterparts, removing duplicates
groupToRemote local remote =
  map (\(p,l) -> (fst <$> findUpdate l remote,(p,l))) $
  -- remove the ones that already exist (waste of time to update them)
  filter (\(_,l) -> all ((l/=) . snd) remote) local


groupToForce :: [(FilePath,Monitor)] -> [(Int,Monitor)] -> [(Maybe Int,(FilePath,Monitor))]
-- ^Group filepath monitors by their remote counterparts
groupToForce local remote =
  map (\(p,l) -> (fst <$> findSimilar l remote,(p,l))) local
  where findSimilar resource = listToMaybe . filter (isSimilar resource . snd)
