module Octkeys.SSH.Key where

import           RIO
import qualified RIO.ByteString          as B

import           Crypto.Hash             (Digest, MD5)
import qualified Crypto.Hash             as Crypto
import           Data.ByteArray.Encoding (Base (Base64), convertFromBase)

fingerprint :: ByteString -> Maybe (Digest MD5)
fingerprint content = do
  body <- pubkeyBody content
  bin  <- decode body
  pure $ Crypto.hash bin

pubkeyBody :: ByteString -> Maybe ByteString
pubkeyBody content =
  case B.split (B.index " " 0) content of
    [header, body] | header == "ssh-rsa" && "AAAA" `B.isPrefixOf` body ->
        Just body
    _ ->
        Nothing

decode :: ByteString -> Maybe ByteString
decode body =
  case convertFromBase Base64 body of
    Right bin | prefix `B.isPrefixOf` bin ->
        Just bin
    _ ->
        Nothing
  where
    prefix = "\NUL\NUL\NUL\assh-rsa"
