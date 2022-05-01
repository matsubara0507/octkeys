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
md5 = fmap show . (fingerprint @MD5)

sha256 :: ByteString -> Maybe String
sha256 = fmap (filter (/= '=') . T.unpack . textDisplay . displayBytesUtf8 . encode) . fingerprint
  where
    encode :: Digest SHA256 -> ByteString
    encode = convertToBase Base64

fingerprint :: HashAlgorithm a => ByteString -> Maybe (Digest a)
fingerprint content = do
  (prefix, body) <- pubkeyBody content
  bin <- decode prefix body
  pure $ Crypto.hash bin

pubkeyBody :: ByteString -> Maybe (ByteString, ByteString)
pubkeyBody content =
  case B.split 32 content of
    ["ssh-rsa", body] | "AAAA" `B.isPrefixOf` body ->
        Just ("\NUL\NUL\NUL\assh-rsa", body)
    ["ssh-ed25519", body] | "AAAA" `B.isPrefixOf` body ->
        Just ("\NUL\NUL\NUL\vssh-ed25519", body)
    _ ->
        Nothing

decode :: ByteString -> ByteString -> Maybe ByteString
decode prefix body =
  case convertFromBase Base64 body of
    Right bin | prefix `B.isPrefixOf` bin ->
        Just bin
    _ ->
        Nothing
