module Octkeys.Env where

import           RIO

import           Data.Extensible
import qualified Data.Yaml       as Y

type Env = Record
  '[ "logger" >: LogFunc
   , "dotssh" >: FilePath
   , "keys"   >: KeyConfig
   ]

type FingerPrint = String

type AccountName = Text

type KeyConfig = Record
  '[ "default" >: [String]
   , "github"  >: Map AccountName FingerPrint
   ]

readConfig :: MonadIO m => FilePath -> m KeyConfig
readConfig = Y.decodeFileThrow
