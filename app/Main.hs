module Main where

import           Paths_octkeys          (version)
import           RIO
import           RIO.Directory          (getHomeDirectory)
import           RIO.FilePath           (dropFileName, takeFileName)

import           Configuration.Dotenv   (defaultConfig, loadFile)
import           Data.Extensible
import           Data.Extensible.GetOpt
import           GetOpt                 (withGetOpt')
import           Mix
import           Mix.Plugin.Logger      as MixLogger
import           Octkeys.Cmd
import           Octkeys.Env
import           System.FSNotify        (Event (Modified))
import qualified System.FSNotify        as FSNotify
import qualified Version

main :: IO ()
main = withGetOpt' "[options] [input-file]" opts $ \r args usage -> do
  _ <- tryIO $ loadFile defaultConfig
  let path = fromMaybe ".octkeys.yaml" $ listToMaybe args
  if | r ^. #help    -> hPutBuilder stdout (fromString usage)
     | r ^. #version -> hPutBuilder stdout (Version.build version)
     | r ^. #watch   -> runCmd r `withWatch` path
     | otherwise     -> runCmd r path
  where
    opts = #help    @= helpOpt
        <: #version @= versionOpt
        <: #verbose @= verboseOpt
        <: #dotssh  @= dotsshOpt
        <: #watch   @= watchOpt
        <: nil

type Options = Record
  '[ "help"    >: Bool
   , "version" >: Bool
   , "verbose" >: Bool
   , "dotssh"  >: (Maybe FilePath)
   , "watch"   >: Bool
   ]

helpOpt :: OptDescr' Bool
helpOpt = optFlag ['h'] ["help"] "Show this help text"

versionOpt :: OptDescr' Bool
versionOpt = optFlag [] ["version"] "Show version"

verboseOpt :: OptDescr' Bool
verboseOpt = optFlag ['v'] ["verbose"] "Enable verbose mode: verbosity level \"debug\""

dotsshOpt :: OptDescr' (Maybe FilePath)
dotsshOpt = optLastArg ['F'] ["dotssh"] "PATH" "ssh config directory path instead of ~/.ssh"

watchOpt :: OptDescr' Bool
watchOpt = optFlag [] ["watch"] "Enable watch mode"

runCmd :: Options -> FilePath -> IO ()
runCmd opts path = do
  config <- readConfig path
  dotssh <- dotsshDirectory (opts ^. #dotssh)
  let plugin = hsequence
             $ #logger <@=> MixLogger.buildPlugin logOpts
            <: #dotssh <@=> pure dotssh
            <: #keys   <@=> pure config
            <: nil
  Mix.run plugin cmd
  where
    logOpts = #handle @= stdout <: #verbose @= (opts ^. #verbose) <: nil

dotsshDirectory :: MonadIO m => Maybe FilePath -> m FilePath
dotsshDirectory (Just path) = pure path
dotsshDirectory Nothing     = (<> "/.ssh") <$> getHomeDirectory

withWatch :: (FilePath -> IO ()) -> FilePath -> IO ()
withWatch act path = do
  act path
  FSNotify.withManager $ \mgr -> do
    _ <- FSNotify.watchDir mgr dir (const True) act'
    forever $ threadDelay 1_000_000
  where
    (dir, fileName) = (dropFileName path, takeFileName path)
    act' event = case event of
      Modified path' _ _ | fileName == takeFileName path' -> act path
      _                                                   -> pure ()
