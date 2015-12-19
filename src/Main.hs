module Main (main) where

import Control.Exception (IOException, catch)
import Control.Monad (when)

import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Either
import Data.List (isSuffixOf)
import Data.Maybe

import Network.HTTP.Client hiding (path)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import Text.Printf (hPrintf)
import Text.Regex.Base
import Text.Regex.TDFA

import Options.Applicative

import System.Directory
import System.FilePath
import System.Environment (getEnv)
import System.Exit
import System.IO

import Resource


exceptIO :: a -> IOException -> a
-- ^Helper `catch` function that only processes IOExceptions
exceptIO = const


-- ARGUMENTS for command-line parsing

data Arguments = Arguments Bool Command

data Command = SaveCommand String [String]
             | LoadCommand [String]

saveCommand :: Parser Command
saveCommand =
  SaveCommand <$>
  strArgument (metavar "DEST" <>
               help "File/Directory in which to store configuration"
              ) <*>
  some (strArgument (metavar "URL..." <>
                     help "Endpoint from which to pull configuration"
                    ))

loadCommand :: Parser Command
loadCommand =
  LoadCommand <$>
  some (strArgument (metavar "SOURCE..." <>
                     help "File/Directory from which to read configuration"
                    ))

arguments :: Parser Arguments
arguments =
  Arguments <$>
  switch (long "dry-run" <> short 'd' <> help "Do not perform any changes") <*>
  subparser (
    command "save" (info (helper <*> saveCommand)
                    (progDesc "Sync Datadog configurations to the local filesystem")) <>
    command "load" (info (helper <*> loadCommand)
                    (progDesc "Sync local filesystem configurations to Datadog")))

parser :: ParserInfo Arguments
parser = info (helper <*> arguments)
         (progDesc "Synchronize between Datadog and the local filesystem")


-- LOCAL FILESYSTEM LOADING functions for loading monitors

loadLocalConfigFromFile :: FilePath -> IO [Either String (FilePath,Monitor)]
-- ^Blindly attempt to read a monitor(s) from a file
loadLocalConfigFromFile path = do
  contents <- LBS.readFile path
  let decodedSingle = (:[]) <$> eitherDecode contents :: Either String [Monitor]
  let decodedMulti = eitherDecode contents :: Either String [Monitor]
  let decodedSingleOld = (\(OldMonitor (DatadogMonitor m)) -> [m]) <$> eitherDecode contents
  let decodedMultiOld = map (\(OldMonitor (DatadogMonitor m)) -> m) <$> eitherDecode contents
  let decoded = decodedSingle <|> decodedMulti <|> decodedSingleOld <|> decodedMultiOld <|> Left ("Could not decode to a monitor: " ++ path)
  return $ if ".json" `isSuffixOf` path
           then either ((:[]) . Left) (map (\x -> Right (path,x))) decoded
           else []

loadLocalConfigFromDir :: FilePath -> IO [Either String (FilePath,Monitor)]
-- ^Blindly attempt to read a monitor(s) from a directory
loadLocalConfigFromDir path =
  ((concat <$>) . mapM loadLocalConfig) =<<
  map (path </>) <$>
  filter ((/='.') . head) <$>
  getDirectoryContents path

loadLocalConfig :: FilePath -> IO [Either String (FilePath,Monitor)]
-- ^Attempt to read a monitor(s) from a path on the filesystem
loadLocalConfig path =
  catch (loadLocalConfigFromFile path) $ exceptIO $
  catch (loadLocalConfigFromDir path) $ exceptIO $
  return [Left ("Cannot access file for reading: " ++ path)]


-- REMOTE DATADOG LOADING functions for loading monitors from Datadog

loadRemoteMonitors :: Manager -> (String,String) -> Maybe Int -> IO [(Int,Monitor)]
loadRemoteMonitors manager (api,app) = maybe loadAll loadOne
  where loadOne = fmap (:[]) . getFromDatadog manager (api,app)
        loadAll = getAllFromDatadog manager (api,app)

loadRemoteConfig :: Manager -> (String,String) -> String -> IO [Either String (Int,Monitor)]
-- ^Attempt to load a monitor(s) based on a Datadog URL
loadRemoteConfig manager (api,app) url
  | url =~ "monitors" =
      let monitorId = fmap read $ listToMaybe $ mrSubList (url =~ "monitors#([0-9]+)")
      in catch (map Right <$> loadRemoteMonitors manager (api,app) monitorId)
         -- Only catch HttpExceptions by using a type cast
         (\e -> return [Left ("Failure loading " ++ url ++ ": " ++ show (e :: HttpException))])
  | otherwise = return [Left ("Could not determine resource for " ++ url)]


-- LOADING FUNCTIONS

gatherMonitors :: Manager -> (String,String) -> [FilePath] -> [String] -> IO ([(String,Monitor)],[(Int,Monitor)])
-- ^Attempt to collect all the monitors from the local filesystem and Datadog
gatherMonitors manager (api,app) localPaths remoteURLs = do
  (localErrors, localMonitors) <- (partitionEithers . concat) <$>
                                  mapM loadLocalConfig localPaths
  mapM_ (hPutStrLn stderr . ("ERROR: "++)) localErrors
  (remoteErrors, remoteMonitors) <- (partitionEithers . concat) <$>
                                    mapM (loadRemoteConfig manager (api,app)) remoteURLs
  mapM_ (hPutStrLn stderr . ("ERROR: "++)) remoteErrors
  let similarLocal = pairSimilar localMonitors
  let similarRemote = pairSimilar remoteMonitors
  mapM_ (\((ia,ra),(ib,rb)) ->
          hPrintf stderr
          "ERROR: Monitor %s in file %s duplicates monitor %s in file %s\n"
          (show ra)
          ia
          (show rb)
          ib
        ) similarLocal
  mapM_ (\((ia,ra),(ib,rb)) ->
          hPrintf stderr
          "ERROR: Monitor %s (%s) duplicates monitor %s (%s)\n"
          (show ia)
          (show ra)
          (show ib)
          (show rb)
        ) similarRemote
  when (length similarLocal + length similarRemote > 0) exitFailure
  when (length localErrors + length remoteErrors > 0) exitFailure
  return (localMonitors, remoteMonitors)


-- LOCAL FILESYSTEM SAVING functions for saving monitors

writeToPathFileDry :: FilePath -> (Maybe Int,Monitor) -> IO ()
-- ^Simulate a successful monitor file write
writeToPathFileDry _ (Nothing,_) = return ()
writeToPathFileDry path (Just c,monitor) =
  hPrintf stdout
  "INFO: Would have written monitor %s (%d) to file: %s\n"
  (show monitor) c path

writeToPathDirDry :: FilePath -> (Maybe Int,Monitor) -> IO ()
-- ^Simulate a successful monitor directory write
writeToPathDirDry _ (Nothing,_) = return ()
writeToPathDirDry path (Just c,monitor) =
  hPrintf stdout
  "INFO: Would have written monitor %s (%d) to new file in directory: %s\n"
  (show monitor) c path

writeToPathsDry :: [(String,[(Maybe Int,Monitor)])] -> IO Bool
-- ^Simulate writing monitors to their respective files
writeToPathsDry [] = return True
writeToPathsDry ((_,[]):xs) = writeToPathsDry xs
writeToPathsDry ((path,monitors):xs) = do
  isDir <- doesDirectoryExist path
  mapM_ ((if isDir then writeToPathDirDry else writeToPathFileDry) path) monitors
  writeToPathsDry xs

writeToPathFile :: FilePath -> [(Maybe Int, Monitor)] -> IO ()
-- ^Attempt to write monitors to a file
-- May raise IOException
writeToPathFile path monitors = do
  let bytes = encodePrettyMonitor $ map snd monitors
  LBS.writeFile path (LBS.snoc bytes 10) -- append newline ('\n')
  let message = "INFO: Monitor %s (%s) written to %s\n"
  mapM_ (\(mi,r) -> maybe (return ()) (\i -> hPrintf stdout message (show i) (show r) path) mi) monitors

writeToPathDir :: FilePath -> [(Maybe Int, Monitor)] -> IO Bool
-- ^Attempt to write monitors each to their own file within a directory
writeToPathDir path monitors = do
  let actionable = filter (isJust . fst) monitors
  let tryFile (c,(mi,r)) = let fpath = path </> c <.> "json"
                           in catch (writeToPathFile fpath [(mi,r)] >> return True)
                              (exceptIO (hPutStrLn stderr ("ERROR: Could not write to file: " ++ fpath) >> return False))
  fmap and $ mapM tryFile $ zip (map show [(1::Int)..]) actionable

writeToPaths :: [(String,[(Maybe Int, Monitor)])] -> IO Bool
-- ^Attempt to write monitors to their respective files
writeToPaths [] = return True
writeToPaths ((_,[]):xs) = writeToPaths xs
writeToPaths ((path,monitors):xs) = do
  let tryFile = catch (writeToPathFile path monitors >> return True)
  let tryDir = catch (writeToPathDir path monitors)
  -- Only catch IOExceptions by using a type cast
  success <- tryFile $ exceptIO $ tryDir $ exceptIO (hPutStrLn stderr ("ERROR: Could not write to path: " ++ path) >> return False)
  (success &&) <$> writeToPaths xs


-- REMOTE DATADOG SAVING functions for saving monitors

writeToDatadogDry :: [(Maybe Int,(FilePath,Monitor))] -> IO Bool
-- ^Simulate writing monitors to Datadog
writeToDatadogDry [] = return True
writeToDatadogDry ((mc,(path,monitor)):xs) = do
  let createMessage = hPrintf stdout
                      "INFO: Would have created new monitor %s from %s in Datadog\n"
                      (show monitor) path
  let updateMessage c = hPrintf stdout
                        "INFO: Would have updated Datadog ID %d with monitor %s from %s\n"
                        c (show monitor) path
  maybe createMessage updateMessage mc
  writeToDatadogDry xs

writeToDatadog :: Manager -> (String,String) -> [(Maybe Int,(FilePath,Monitor))] -> IO Bool
-- ^Attempt to write monitors to Datadog
writeToDatadog _ _ [] = return True
writeToDatadog manager (api,app) ((mc,(path,monitor)):xs) = do
  let createMessage = hPrintf stdout
                      "INFO: Created new monitor %s from %s as %d in Datadog\n"
                      (show monitor) path
  let updateMessage c = hPrintf stdout
                        "INFO: Updated Datadog ID %d with monitor %s from %s\n"
                        c (show monitor) path
  let message = if isNothing mc then createMessage else updateMessage
  let errorMessage e = hPrintf stderr
                       "ERROR: Could not send monitor %s from %s to Datadog: %s\n"
                       (show monitor) path (show (e :: HttpException))
  success <- catch (sendToDatadog manager (api,app) mc monitor >>= message >> return True)
             (\e -> errorMessage e >> return False)
  (success &&) <$> writeToDatadog manager (api,app) xs


-- MAIN

loadKeysFromEnv :: IO (String,String)
loadKeysFromEnv = do
  api <- getEnv "DATADOG_API_KEY"
  app <- getEnv "DATADOG_APP_KEY"
  return (api,app)

run :: Arguments -> IO Bool
run (Arguments dryrun (SaveCommand localPath remoteURLs)) = do
  manager <- newManager tlsManagerSettings
  apiapp <- loadKeysFromEnv
  (localMonitors, remoteMonitors) <- gatherMonitors manager apiapp [localPath] remoteURLs
  let actions = groupToFilePath localPath remoteMonitors localMonitors
  (if dryrun then writeToPathsDry else writeToPaths) actions
run (Arguments dryrun (LoadCommand localPaths)) = do
  manager <- newManager tlsManagerSettings
  apiapp <- loadKeysFromEnv
  let allRemoteURLs = ["monitors"]
  (localMonitors, remoteMonitors) <- gatherMonitors manager apiapp localPaths allRemoteURLs
  let actions = groupToRemote localMonitors remoteMonitors
  (if dryrun then writeToDatadogDry else writeToDatadog manager apiapp) actions


main :: IO ()
main = execParser parser >>=
       run >>=
       (\success -> if success then exitSuccess else exitFailure)
