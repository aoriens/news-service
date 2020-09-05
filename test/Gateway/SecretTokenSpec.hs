module Gateway.SecretTokenSpec
  ( spec
  ) where

import qualified Core.Authentication as Auth
import qualified Data.ByteString as B
import qualified Gateway.SecretToken as G
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "generateIO" $ do
    it
      "should return hash that is checked successfully against the generated token" $ do
      state <- G.initIOState
      (token, hash) <- G.generateIO defaultConfig state
      let matches = G.tokenMatchesHash token hash
      matches `shouldBe` True
    it "should return a different token and hash on a next invocation" $ do
      state <- G.initIOState
      (token1, hash1) <- G.generateIO defaultConfig state
      (token2, hash2) <- G.generateIO defaultConfig state
      token1 `shouldNotBe` token2
      hash1 `shouldNotBe` hash2
    it "should return token of the specified length" $
      property $ \(NonNegative expectedLen) -> do
        state <- G.initIOState
        let config = G.Config {cfTokenLength = expectedLen}
        (Auth.SecretToken token, _) <- G.generateIO config state
        B.length token `shouldBe` expectedLen

defaultConfig :: G.Config
defaultConfig = G.Config {cfTokenLength = 8}
