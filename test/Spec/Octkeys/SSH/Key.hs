module Spec.Octkeys.SSH.Key (spec) where

import           RIO
import qualified RIO.ByteString   as B

import qualified Octkeys.SSH.Key  as SSHKey
import           Test.Tasty.Hspec

spec :: Spec
spec = do
  describe "fingerprint" $ do
    context "when apply correct ssh public key" $
      it "should return Just fingerprint" $
        show <$> SSHKey.fingerprint sshkey `shouldBe` Just (filter (/= ':') fp)
    context "when apply incorrect header publick key" $ do
      let sshkey' = B.drop 1 sshkey
      it "should return Nothing" $
        show <$> SSHKey.fingerprint sshkey' `shouldBe` Nothing
    context "when apply public key without header" $ do
      let sshkey' = B.drop (B.length "ssh-rsa ") sshkey
      it "should return Nothing" $
        show <$> SSHKey.fingerprint sshkey' `shouldBe` Nothing
    context "when incorrect public key (no base64)" $ do
      let sshkey' = sshkey <> "1"
      it "should return Nothing" $
        show <$> SSHKey.fingerprint sshkey' `shouldBe` Nothing


sshkey :: ByteString
sshkey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDlpea3sCHIlkER6XA1ObytOWK/YTEEdkFtbMuDyrsui/cLXEFEkCotLz30uzER4fR0hJH0Ro/LbwaE7hvJkxH414+leSnUld7jchFUzZa2KPREyETBv0w0hliI+4MFm6e9zyr0hWBDOO4HFg4dMDuY2x4uXCHeAEVTXWlB23hqSoJBQLbNh/zFMzmjMP1DViA1gIVZoVeNfYEnL/eEmMY3wSo1QcjaHWZWbdvJceVz+W32LdoONP2jwAhHN2w57JMB4uiTKN1iFkEXSP1PJgv/vaWDearcJc5VLvFEtDLYklWo6OM9Sa27V3WR4n9uhnzYiheyE81NJeZe4O9JjFnJhQamjBfF/Sil4I3zUMHrvTzRjexZ3JOiGim7qFn1M7kIZNq7O/gkz6K0z/a4k4HF/9CmW6dXieuo0zYfjgYSNySTapAfbAdyoGS47K75XY2PfzzMS2CpdQKk+M3oYJcdFyldKgDXgFUJu7SzUzS/Tb/nrjRA81I8JU+liiNulIWmxAILcd6pL+f405G05rAoa4qdOLvUJwbR8MVlsD6Fu7ef3AZkTos+F6q0kvnk+EzKgpwmKoCaD2S7HKpnxLSxQtOzYMBayuPuLLHK+2cmHTbNlQd4XQJgkfhKXYRjMd0Qkp1oHTPd53qeBzOxQhmm6tjD8rp+A/c7IGcM2/NKZw=="

fp :: String
fp = "f3:b1:76:72:02:0e:4e:8a:d2:55:16:03:4f:ac:c1:2d"
