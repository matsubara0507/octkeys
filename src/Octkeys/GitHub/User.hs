module Octkeys.GitHub.User where

import           RIO

import           Data.Extensible
import           Network.HTTP.Req (GET (..), https, req, runReq, (/:))
import qualified Network.HTTP.Req as Req

type Key = Record
  '[ "id"  >: Int
   , "key" >: String
   ]

fetchKeys :: MonadIO m => Text -> m [Key]
fetchKeys name = runReq Req.defaultHttpConfig $ do
  resp <- req GET url Req.NoReqBody Req.jsonResponse header
  pure $ Req.responseBody resp
  where
    url = https "api.github.com" /: "users" /: name /: "keys"
    header = Req.header "User-Agent" "octkeys"
