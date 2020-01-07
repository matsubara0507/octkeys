module Main where

import           RIO

import qualified Spec.Octkeys.GitHub.User
import qualified Spec.Octkeys.SSH.Key
import           Test.Tasty
import           Test.Tasty.Hspec

main :: IO ()
main = defaultMain =<< spec

spec :: IO TestTree
spec = testGroup "Octkeys" <$> sequence
  [ testSpec "Octkeys.SSH.Key" Spec.Octkeys.SSH.Key.spec
  , testSpec "Octkeys.GitHub.User" Spec.Octkeys.GitHub.User.spec
  ]
