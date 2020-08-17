{-# LANGUAGE RecordWildCards #-}

module Gateway.SecretToken
  ( generate
  , initState
  , tokenMatchesHash
  , State
  , Config(..)
  , HashAlgorithm(..)
  , SecretTokenInfo(..)
  ) where

import qualified Crypto.Hash as Hash
import qualified Crypto.Random as Random
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS

data Config =
  Config
    { cfTokenLength :: Int
    , cfHashAlgorithm :: HashAlgorithm
    }
  deriving (Eq, Show)

newtype State =
  State Random.ChaChaDRG

data HashAlgorithm =
  HashAlgorithmSHA256
  deriving (Eq, Show, Bounded, Enum)

data SecretTokenInfo =
  SecretTokenInfo
    { stiToken :: BS.ByteString
    , stiHash :: BS.ByteString
    , stiHashAlgorithm :: HashAlgorithm
    }

initState :: IO State
initState = State <$> Random.drgNew

generate :: State -> Config -> (SecretTokenInfo, State)
generate (State oldGen) Config {..} =
  ( SecretTokenInfo
      { stiToken = token
      , stiHash = hashWithAlgorithm cfHashAlgorithm token
      , stiHashAlgorithm = cfHashAlgorithm
      }
  , State newGen)
  where
    (token, newGen) =
      Random.withDRG oldGen $ Random.getRandomBytes cfTokenLength

hashWithAlgorithm :: HashAlgorithm -> BS.ByteString -> BS.ByteString
hashWithAlgorithm HashAlgorithmSHA256 bytes =
  BA.convert $ Hash.hashWith Hash.SHA256 bytes

tokenMatchesHash :: SecretTokenInfo -> Bool
tokenMatchesHash SecretTokenInfo {..} =
  hashWithAlgorithm stiHashAlgorithm stiToken == stiHash
