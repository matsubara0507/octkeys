{-# LANGUAGE TypeApplications #-}

module Octkeys.SSH.Key where

import           RIO
import qualified RIO.ByteString          as B
import qualified RIO.Text                as T

import           Crypto.Hash             (Digest, HashAlgorithm, MD5, SHA256)
import qualified Crypto.Hash             as Crypto
import           Data.ByteArray.Encoding (Base (Base64), convertFromBase,
                                          convertToBase)

md5 :: ByteString -> Maybe String
md5 = fmap show . (fingerprint @ MD5)

sha256 :: ByteString -> Maybe String
sha256 = fmap (filter (/= '=') . T.unpack . textDisplay . displayBytesUtf8 . encode) . fingerprint
  where
    encode :: Digest SHA256 -> ByteString
    encode = convertToBase Base64

fingerprint :: HashAlgorithm a => ByteString -> Maybe (Digest a)
fingerprint content = do
  body <- pubkeyBody content
  bin  <- decode body
  pure $ Crypto.hash bin

pubkeyBody :: ByteString -> Maybe ByteString
pubkeyBody content =
  case B.split 32 content of
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
