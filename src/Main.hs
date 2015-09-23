module Main (main) where

import Control.Exception (IOException, catch)
import Control.Monad (when)

import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy as LBS
import Data.Either
import Data.Maybe

import qualified Network.Datadog as Datadog
import qualified Network.Datadog.Monitor as Datadog.Monitor
import qualified Network.Datadog.Types as Datadog.Types

import Network.HTTP.Client (HttpException)

import Text.Printf (hPrintf)
import Text.Regex.Base
import Text.Regex.TDFA

import Options.Applicative

import System.Directory
import System.FilePath
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


-- LOCAL FILESYSTEM LOADING functions for loading resources

loadLocalConfigFromFile :: FilePath -> IO [Either String (FilePath,Resource)]
-- ^Blindly attempt to read a resource(s) from a file
loadLocalConfigFromFile path = do
  contents <- LBS.readFile path
  let decodedSingle = (:[]) <$> eitherDecode contents :: Either String [Resource]
  let decodedMulti = eitherDecode contents :: Either String [Resource]
  let decoded = decodedSingle <|> decodedMulti <|> Left ("Could not decode to a resource: " ++ path)
  return $ either ((:[]) . Left) (map (\x -> Right (path,x))) decoded

loadLocalConfigFromDir :: FilePath -> IO [Either String (FilePath,Resource)]
-- ^Blindly attempt to read a resource(s) from a directory
loadLocalConfigFromDir path =
  ((concat <$>) . mapM loadLocalConfig) =<<
  map (path </>) <$>
  filter ((/='.') . head) <$>
  getDirectoryContents path

loadLocalConfig :: FilePath -> IO [Either String (FilePath,Resource)]
-- ^Attempt to read a resource(s) from a path on the filesystem
loadLocalConfig path =
  catch (loadLocalConfigFromFile path) $ exceptIO $
  catch (loadLocalConfigFromDir path) $ exceptIO $
  return [Left ("Cannot access file for reading: " ++ path)]


-- REMOTE DATADOG LOADING functions for loading resources from Datadog

loadRemoteMonitors :: Datadog.Types.Environment -> Maybe Datadog.Monitor.MonitorId -> IO [(Int,Resource)]
-- ^Attempt to load a monitor(s) from Datadog
loadRemoteMonitors env mId =
  map (\m -> (Datadog.Monitor.monitorId' m,Monitor (Datadog.Monitor.monitorSpec m))) <$>
  maybe loadAll loadOne mId
  where loadOne = fmap (:[]) . Datadog.Monitor.loadMonitor env
        loadAll = Datadog.Monitor.loadMonitors env []

loadRemoteConfig :: Datadog.Types.Environment -> String -> IO [Either String (Int,Resource)]
-- ^Attempt to load a resource(s) based on a Datadog URL
loadRemoteConfig env url
  | url =~ "monitors" =
      let monitorId = fmap read $ listToMaybe $ mrSubList (url =~ "monitors#([0-9]+)")
      in catch (map Right <$> loadRemoteMonitors env monitorId)
         -- Only catch HttpExceptions by using a type cast
         (\e -> return [Left ("Failure loading " ++ url ++ ": " ++ show (e :: HttpException))])
  | otherwise = return [Left ("Could not determine resource for " ++ url)]


-- LOADING FUNCTIONS

gatherResources :: Datadog.Types.Environment -> [FilePath] -> [String] -> IO ([(String,Resource)],[(Int,Resource)])
-- ^Attempt to collect all the resources from the local filesystem and Datadog
gatherResources env localPaths remoteURLs = do
  (localErrors, localResources) <- (partitionEithers . concat) <$>
                                   mapM loadLocalConfig localPaths
  mapM_ (hPutStrLn stderr . ("WARNING: "++)) localErrors
  (remoteErrors, remoteResources) <- (partitionEithers . concat) <$>
                                     mapM (loadRemoteConfig env) remoteURLs
  mapM_ (hPutStrLn stderr . ("WARNING: "++)) remoteErrors
  let similarLocal = pairSimilar localResources
  let similarRemote = pairSimilar remoteResources
  mapM_ (\((ia,ra),(ib,rb)) ->
          hPrintf stderr
          "ERROR: Resource %s in file %s duplicates resource %s in file %s\n"
          (show ra)
          ia
          (show rb)
          ib
        ) similarLocal
  mapM_ (\((ia,ra),(ib,rb)) ->
          hPrintf stderr
          "ERROR: Resource %s (%s) duplicates resource %s (%s)\n"
          (show ia)
          (show ra)
          (show ib)
          (show rb)
        ) similarRemote
  when (length similarLocal + length similarRemote > 0) exitFailure
  return (localResources, remoteResources)


-- LOCAL FILESYSTEM SAVING functions for saving resources

writeToPathFileDry :: FilePath -> (Maybe Int,Resource) -> IO ()
-- ^Simulate a successful resource file write
writeToPathFileDry _ (Nothing,_) = return ()
writeToPathFileDry path (Just c,resource) =
  hPrintf stdout
  "INFO: Would have written resource %s (%d) to file: %s\n"
  (show resource) c path

writeToPathDirDry :: FilePath -> (Maybe Int,Resource) -> IO ()
-- ^Simulate a successful resource directory write
writeToPathDirDry _ (Nothing,_) = return ()
writeToPathDirDry path (Just c,resource) =
  hPrintf stdout
  "INFO: Would have written resource %s (%d) to new file in directory: %s\n"
  (show resource) c path

writeToPathsDry :: [(String,[(Maybe Int,Resource)])] -> IO Bool
-- ^Simulate writing resources to their respective files
writeToPathsDry [] = return True
writeToPathsDry ((_,[]):xs) = writeToPathsDry xs
writeToPathsDry ((path,resources):xs) = do
  isDir <- doesDirectoryExist path
  mapM_ ((if isDir then writeToPathDirDry else writeToPathFileDry) path) resources
  writeToPathsDry xs

writeToPathFile :: FilePath -> [(Maybe Int, Resource)] -> IO ()
-- ^Attempt to write resources to a file
-- May raise IOException
writeToPathFile path resources = do
  let bytes = if length resources > 1
              then encodePretty $ map snd resources
              else encodePretty $ head $ map snd resources
  LBS.writeFile path (LBS.snoc bytes 10) -- append newline ('\n')
  let message = "INFO: Resource %s (%s) written to %s\n"
  mapM_ (\(mi,r) -> maybe (return ()) (\i -> hPrintf stdout message (show i) (show r) path) mi) resources

writeToPathDir :: FilePath -> [(Maybe Int, Resource)] -> IO Bool
-- ^Attempt to write resources each to their own file within a directory
writeToPathDir path resources = do
  let actionable = filter (isJust . fst) resources
  let tryFile (c,(mi,r)) = let fpath = path </> c <.> "json"
                           in catch (writeToPathFile fpath [(mi,r)] >> return True)
                              (exceptIO (hPutStrLn stderr ("ERROR: Could not write to file: " ++ fpath) >> return False))
  fmap and $ mapM tryFile $ zip (map show [(1::Int)..]) actionable

writeToPaths :: [(String,[(Maybe Int, Resource)])] -> IO Bool
-- ^Attempt to write resources to their respective files
writeToPaths [] = return True
writeToPaths ((_,[]):xs) = writeToPaths xs
writeToPaths ((path,resources):xs) = do
  let tryFile = catch (writeToPathFile path resources >> return True)
  let tryDir = catch (writeToPathDir path resources)
  -- Only catch IOExceptions by using a type cast
  success <- tryFile $ exceptIO $ tryDir $ exceptIO (hPutStrLn stderr ("ERROR: Could not write to path: " ++ path) >> return False)
  (success &&) <$> writeToPaths xs


-- REMOTE DATADOG SAVING functions for saving resources

writeToDatadogDry :: [(Maybe Int,(FilePath,Resource))] -> IO Bool
-- ^Simulate writing resources to Datadog
writeToDatadogDry [] = return True
writeToDatadogDry ((mc,(path,resource)):xs) = do
  let createMessage = hPrintf stdout
                      "INFO: Would have created new resource %s from %s in Datadog\n"
                      (show resource) path
  let updateMessage c = hPrintf stdout
                        "INFO: Would have updated Datadog ID %d with resource %s from %s\n"
                        c (show resource) path
  maybe createMessage updateMessage mc
  writeToDatadogDry xs

writeToDatadog :: Datadog.Types.Environment -> [(Maybe Int,(FilePath,Resource))] -> IO Bool
-- ^Attempt to write resources to Datadog
writeToDatadog _ [] = return True
writeToDatadog env ((mc,(path,resource)):xs) = do
  let createMessage = hPrintf stdout
                      "INFO: Created new resource %s from %s as %d in Datadog\n"
                      (show resource) path
  let updateMessage c = hPrintf stdout
                        "INFO: Updated Datadog ID %d with resource %s from %s\n"
                        c (show resource) path
  let message = if isNothing mc then createMessage else updateMessage
  let errorMessage e = hPrintf stderr
                       "ERROR: Could not send resource %s from %s to Datadog: %s\n"
                       (show resource) path (show (e :: HttpException))
  success <- catch (sendToDatadog env mc resource >>= message >> return True)
             (\e -> errorMessage e >> return False)
  (success &&) <$> writeToDatadog env xs


-- MAIN

run :: Arguments -> IO Bool
run (Arguments dryrun (SaveCommand localPath remoteURLs)) = do
  env <- Datadog.loadKeysFromEnv >>= Datadog.createEnvironment
  (localResources, remoteResources) <- gatherResources env [localPath] remoteURLs
  let actions = groupToFilePath localPath remoteResources localResources
  (if dryrun then writeToPathsDry else writeToPaths) actions
run (Arguments dryrun (LoadCommand localPaths)) = do
  env <- Datadog.loadKeysFromEnv >>= Datadog.createEnvironment
  let allRemoteURLs = ["monitors"]
  (localResources, remoteResources) <- gatherResources env localPaths allRemoteURLs
  let actions = groupToRemote localResources remoteResources
  (if dryrun then writeToDatadogDry else writeToDatadog env) actions


main :: IO ()
main = execParser parser >>=
       run >>=
       (\success -> if success then exitSuccess else exitFailure)
