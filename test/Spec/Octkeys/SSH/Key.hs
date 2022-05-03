module Spec.Octkeys.SSH.Key (spec) where

import           RIO
import qualified RIO.ByteString   as B

import qualified Octkeys.SSH.Key  as SSHKey
import           Test.Hspec

spec :: Spec
spec = do
  describe "md5" $ do
    context "when apply correct ssh public key" $
      it "should return Just fingerprint" $
        SSHKey.md5 sshkey1 `shouldBe` Just (filter (/= ':') md5fp1)
    context "when apply correct ssh public key (ed25519)" $
      it "should return Just fingerprint" $
        SSHKey.md5 sshkey2 `shouldBe` Just (filter (/= ':') md5fp2)
    context "when apply incorrect header publick key" $ do
      let sshkey' = B.drop 1 sshkey1
      it "should return Nothing" $
        SSHKey.md5 sshkey' `shouldBe` Nothing
    context "when apply public key without header" $ do
      let sshkey' = B.drop (B.length "ssh-rsa ") sshkey1
      it "should return Nothing" $
        SSHKey.md5 sshkey' `shouldBe` Nothing
    context "when incorrect public key (no base64)" $ do
      let sshkey' = sshkey1 <> "1"
      it "should return Nothing" $
        SSHKey.md5 sshkey' `shouldBe` Nothing
  describe "sha256" $ do
    context "when apply correct ssh public key (rsa)" $
      it "should return Just fingerprint" $
        SSHKey.sha256 sshkey1 `shouldBe` Just (drop 7 sha256fp1)
    context "when apply correct ssh public key (ed25519)" $
      it "should return Just fingerprint" $
        SSHKey.sha256 sshkey2 `shouldBe` Just (drop 7 sha256fp2)
    context "when apply incorrect header publick key" $ do
      let sshkey' = B.drop 1 sshkey1
      it "should return Nothing" $
        SSHKey.sha256 sshkey' `shouldBe` Nothing
    context "when apply public key without header" $ do
      let sshkey' = B.drop (B.length "ssh-rsa ") sshkey1
      it "should return Nothing" $
        SSHKey.sha256 sshkey' `shouldBe` Nothing
    context "when incorrect public key (no base64)" $ do
      let sshkey' = sshkey1 <> "1"
      it "should return Nothing" $
        SSHKey.sha256 sshkey' `shouldBe` Nothing

sshkey1, sshkey2 :: ByteString
sshkey1 = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDlpea3sCHIlkER6XA1ObytOWK/YTEEdkFtbMuDyrsui/cLXEFEkCotLz30uzER4fR0hJH0Ro/LbwaE7hvJkxH414+leSnUld7jchFUzZa2KPREyETBv0w0hliI+4MFm6e9zyr0hWBDOO4HFg4dMDuY2x4uXCHeAEVTXWlB23hqSoJBQLbNh/zFMzmjMP1DViA1gIVZoVeNfYEnL/eEmMY3wSo1QcjaHWZWbdvJceVz+W32LdoONP2jwAhHN2w57JMB4uiTKN1iFkEXSP1PJgv/vaWDearcJc5VLvFEtDLYklWo6OM9Sa27V3WR4n9uhnzYiheyE81NJeZe4O9JjFnJhQamjBfF/Sil4I3zUMHrvTzRjexZ3JOiGim7qFn1M7kIZNq7O/gkz6K0z/a4k4HF/9CmW6dXieuo0zYfjgYSNySTapAfbAdyoGS47K75XY2PfzzMS2CpdQKk+M3oYJcdFyldKgDXgFUJu7SzUzS/Tb/nrjRA81I8JU+liiNulIWmxAILcd6pL+f405G05rAoa4qdOLvUJwbR8MVlsD6Fu7ef3AZkTos+F6q0kvnk+EzKgpwmKoCaD2S7HKpnxLSxQtOzYMBayuPuLLHK+2cmHTbNlQd4XQJgkfhKXYRjMd0Qkp1oHTPd53qeBzOxQhmm6tjD8rp+A/c7IGcM2/NKZw=="
sshkey2 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAYHKI7cE/mgafMHRYfxzbW7yEOjrlF5OFqOIqgZHr6w"

md5fp1, sha256fp1, md5fp2, sha256fp2 :: String
md5fp1 = "f3:b1:76:72:02:0e:4e:8a:d2:55:16:03:4f:ac:c1:2d"
sha256fp1 = "SHA256:DpMMXGF56PZIU3dJalq/vu0g6LNZdOy/fcrsJX57l2I"
md5fp2 = "5d:d9:25:dc:77:e6:b5:bb:d2:fe:d9:d3:23:c3:10:cc"
sha256fp2 = "SHA256:elYQ/zBWMcoZv3JMrcuW+a0EMdRRCWfwZuRHmfbguoc"
