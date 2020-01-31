module Octkeys.GitHub.User where

import           RIO

import           Network.HTTP.Req (GET (..), https, req, runReq, (/:))
import qualified Network.HTTP.Req as Req
import qualified RIO.ByteString   as B

fetchKeys :: MonadIO m => Text -> m [ByteString]
fetchKeys name = runReq Req.defaultHttpConfig $ do
  resp <- req GET url Req.NoReqBody Req.bsResponse header
  pure (B.split 10 $ Req.responseBody resp)
  where
    url = https "github.com" /: name <> ".keys"
    header = Req.header "User-Agent" "octkeys"
