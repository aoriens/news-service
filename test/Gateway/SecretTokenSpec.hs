{-# LANGUAGE DisambiguateRecordFields #-}

module Gateway.SecretTokenSpec
  ( spec
  ) where

import Core.Interactor.CreateUser as I
import qualified Data.ByteString as BS
import qualified Gateway.SecretToken as G
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "generateIO" $ do
    it
      "should return hash that is checked successfully against the generated token" $ do
      state <- G.initIOState
      tokenInfo <- G.generateIO defaultConfig state
      let matches = G.tokenMatchesHash tokenInfo
      matches `shouldBe` True
    it "should return a different token and hash on a next invocation" $ do
      state <- G.initIOState
      tokenInfo1 <- G.generateIO defaultConfig state
      tokenInfo2 <- G.generateIO defaultConfig state
      I.stiToken tokenInfo1 `shouldNotBe` I.stiToken tokenInfo2
      I.stiHash tokenInfo1 `shouldNotBe` I.stiHash tokenInfo2
    it "should return token of the specified length" $
      property $ \(NonNegative expectedLen) -> do
        state <- G.initIOState
        let config = G.Config {cfTokenLength = expectedLen}
        tokenInfo <- G.generateIO config state
        BS.length (I.secretTokenBytes $ I.stiToken tokenInfo) `shouldBe`
          expectedLen

defaultConfig :: G.Config
defaultConfig = G.Config {cfTokenLength = 8}
