module Main where

import           Paths_octkeys          (version)
import           RIO
import           RIO.Directory          (getHomeDirectory)

import           Configuration.Dotenv   (defaultConfig, loadFile)
import           Data.Extensible
import           Data.Extensible.GetOpt
import           GetOpt                 (withGetOpt')
import           Mix
import           Mix.Plugin.Logger      as MixLogger
import           Octkeys.Cmd
import           Octkeys.Env
import qualified Version

main :: IO ()
main = withGetOpt' "[options] [input-file]" opts $ \r args usage -> do
  _ <- tryIO $ loadFile defaultConfig
  if | r ^. #help    -> hPutBuilder stdout (fromString usage)
     | r ^. #version -> hPutBuilder stdout (Version.build version)
     | otherwise     -> runCmd r (listToMaybe args)
  where
    opts = #help    @= helpOpt
        <: #version @= versionOpt
        <: #verbose @= verboseOpt
        <: #dotssh  @= dotsshOpt
        <: nil

type Options = Record
  '[ "help"    >: Bool
   , "version" >: Bool
   , "verbose" >: Bool
   , "dotssh"  >: (Maybe FilePath)
   ]

helpOpt :: OptDescr' Bool
helpOpt = optFlag ['h'] ["help"] "Show this help text"

versionOpt :: OptDescr' Bool
versionOpt = optFlag [] ["version"] "Show version"

verboseOpt :: OptDescr' Bool
verboseOpt = optFlag ['v'] ["verbose"] "Enable verbose mode: verbosity level \"debug\""

dotsshOpt :: OptDescr' (Maybe FilePath)
dotsshOpt = optLastArg ['F'] ["dotssh"] "PATH" "ssh config directory path instead of ~/.ssh"

runCmd :: Options -> Maybe FilePath -> IO ()
runCmd opts path = do
  config <- readConfig (fromMaybe ".octkeys.yaml" path)
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
