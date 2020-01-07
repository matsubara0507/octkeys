module Octkeys.Cmd where

import           RIO
import qualified RIO.ByteString      as B
import qualified RIO.List            as L
import qualified RIO.Map             as Map

import qualified Mix.Plugin.Logger   as Mix
import           Octkeys.Env
import qualified Octkeys.GitHub.User as GitHub
import qualified Octkeys.SSH.Key     as SSHKey

type Key = ByteString

cmd :: RIO Env ()
cmd = do
  keyConfig <- asks (view #github . view #keys)
  keys <- forMaybeM (Map.toList keyConfig) $ \(name, allow) -> do
    key <- collectKeys name allow
    when (isNothing key) $
      Mix.logWarn ("not find key: " <> display name)
    pure key
  writeAuthorizedKeys keys

collectKeys :: AccountName -> FingerPrint -> RIO Env (Maybe Key)
collectKeys name allow = tryAny (GitHub.fetchKeys name) >>= \case
  Left err ->
    Mix.logError (buildErrMessage err) >> pure Nothing
  Right keys ->
    pure $ findAllowKey allow (fmap (fromString . view #key) keys)
  where
    buildErrMessage e =
      mconcat [ "cannot fetch ", display name, " keys: ", displayShow e ]

findAllowKey :: FingerPrint -> [Key] -> Maybe Key
findAllowKey allow keys =
  L.find (\key -> fmap show (SSHKey.fingerprint key) == allow') keys
  where
    allow' = Just $ filter (/= ':') allow

writeAuthorizedKeys :: [Key] -> RIO Env ()
writeAuthorizedKeys keys = do
  defaultKeys <- asks (view #default . view #keys)
  path <- (<> "/authorized_keys") <$> asks (view #dotssh)
  B.writeFile path (buildAuthorizedKeys defaultKeys keys)

buildAuthorizedKeys :: [String] -> [Key] -> ByteString
buildAuthorizedKeys defaultKeys keys =
   B.intercalate "\n" (map fromString defaultKeys ++ keys)

showNotImpl :: MonadIO m => m ()
showNotImpl = hPutBuilder stdout "not yet implement command."
