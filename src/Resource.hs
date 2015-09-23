{-# LANGUAGE OverloadedStrings #-}

module Resource
( Resource(..)
, sendToDatadog
, isSimilar
, pairSimilar
, isUpdate
, groupToFilePath
, groupToRemote
) where

import Data.Aeson
import Data.Aeson.Types (modifyFailure)
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (unpack)

import qualified Network.Datadog as DD
import qualified Network.Datadog.Monitor as DD.Monitor


data Resource = Monitor DD.Monitor.MonitorSpec
              deriving (Eq)

instance FromJSON Resource where
  -- Use the first successful parse, and provide a generic error on failure
  parseJSON (Object v) = modifyFailure (const "Could not decode as a Datadog resource") $
                         Monitor <$> v .: "monitor"
  parseJSON _ = fail "Could not decode as a Datadog resource"

instance ToJSON Resource where
  toJSON (Monitor spec) = object ["monitor" .= spec]

instance Show Resource where
  show (Monitor ms) =
    "Monitor: " ++
    maybe "<untitled>" (\x -> "\"" ++ unpack x ++ "\"") (DD.Monitor.monitorSpecName ms)


sendToDatadog :: DD.Environment -> Maybe Int -> Resource -> IO Int
sendToDatadog env mid (Monitor ms) =
  DD.Monitor.monitorId' <$>
  maybe (DD.Monitor.createMonitor env) (DD.Monitor.updateMonitor env) mid ms


isSimilar :: Resource -> Resource -> Bool
-- ^Can a resource be considered an updated version of another?
-- This differs from sameness in that the two resources aren't quite the same,
-- but are similar enough that they can be considered versions of each other.
isSimilar (Monitor a) (Monitor b) =
  DD.Monitor.monitorSpecType' a == DD.Monitor.monitorSpecType' b &&
  (
    DD.Monitor.monitorSpecQuery a == DD.Monitor.monitorSpecQuery b ||
    DD.Monitor.monitorSpecName a == DD.Monitor.monitorSpecName b
    -- TODO: Enable after adding unique descriptions to existing monitorscds
    -- DD.Monitor.monitorSpecMessage a == DD.Monitor.monitorSpecMessage b
  )
isSimilar _ _ = False


pairSimilar :: [(a,Resource)] -> [((a,Resource),(a,Resource))]
-- ^Pair-up resources that are similar.
-- Note that this function avoids duplication, i.e. is (a,b) is in the result,
-- (b,a) will not be.
pairSimilar [] = []
pairSimilar (x:xs) = let ys = filter (isSimilar (snd x) . snd) xs
                     in map (\y -> (x,y)) ys ++ pairSimilar xs


isUpdate :: Resource -> Resource -> Bool
-- ^Can a resource be considered an update of another?
-- This is the same as two resources being similar, but not the same (there is
-- no need for an update if they are the same)
isUpdate a b = isSimilar a b && a /= b


findUpdate :: Resource -> [(b,Resource)] -> Maybe (b,Resource)
-- ^Find a resource that can be associated with an incumbent resource
findUpdate resource = listToMaybe . filter (isUpdate resource . snd)


groupToFilePath :: FilePath -> [(Int,Resource)] -> [(FilePath,Resource)] -> [(FilePath,[(Maybe Int,Resource)])]
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


groupToRemote :: [(FilePath,Resource)] -> [(Int,Resource)] -> [(Maybe Int,(FilePath,Resource))]
groupToRemote local remote =
  map (\(p,l) -> (fst <$> findUpdate l remote,(p,l))) $
  -- remove the ones that already exist (waste of time to update them)
  filter (\(_,l) -> all ((l/=) . snd) remote) local
