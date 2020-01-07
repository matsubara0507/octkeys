module Main where

import           Paths_octkeys          (version)
import           RIO

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
   , "dotssh"  >: FilePath
   ]

helpOpt :: OptDescr' Bool
helpOpt = optFlag ['h'] ["help"] "Show this help text"

versionOpt :: OptDescr' Bool
versionOpt = optFlag [] ["version"] "Show version"

verboseOpt :: OptDescr' Bool
verboseOpt = optFlag ['v'] ["verbose"] "Enable verbose mode: verbosity level \"debug\""

dotsshOpt :: OptDescr' FilePath
dotsshOpt =
  fromMaybe "~/.ssh" <$> optLastArg ['F'] ["dotssh"] "PATH" "ssh config directory path instead of ~/.ssh"

runCmd :: Options -> Maybe FilePath -> IO ()
runCmd opts path = do
  config <- readConfig (fromMaybe ".octkeys.yaml" path)
  let plugin = hsequence
             $ #logger <@=> MixLogger.buildPlugin logOpts
            <: #dotssh <@=> pure (opts ^. #dotssh)
            <: #keys   <@=> pure config
            <: nil
  Mix.run plugin cmd
  where
    logOpts = #handle @= stdout <: #verbose @= (opts ^. #verbose) <: nil
