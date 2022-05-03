module Spec.Octkeys.GitHub.User (spec) where

import           RIO

import qualified Octkeys.GitHub.User as GitHub
import           Test.Hspec
import           Test.Tasty.Hspec

spec :: Spec
spec = do
  describe "fetchKeys" $ do
    context "when apply correct user" $
      it "should return Just keys" $
        hasKey (GitHub.fetchKeys "matsubara0507") `shouldReturn` True
    context "when apply incorrect user" $
      it "should return Nothing" $
        hasKey (GitHub.fetchKeys "") `shouldReturn` False
  where
    hasKey m = tryAny m <&> \case
      Right (_:_) -> True
      _           -> False
