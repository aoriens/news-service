{-# LANGUAGE RecordWildCards #-}

module Gateway.SecretToken
  ( generate
  , initState
  , tokenMatchesHash
  , State
  , Config(..)
  ) where

import qualified Core.Interactor.CreateUser as I
import qualified Crypto.Hash as Hash
import qualified Crypto.Random as Random
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS

data Config =
  Config
    { cfTokenLength :: Int
    , cfHashAlgorithm :: I.HashAlgorithm
    }
  deriving (Eq, Show)

newtype State =
  State Random.ChaChaDRG

initState :: IO State
initState = State <$> Random.drgNew

generate :: State -> Config -> (I.SecretTokenInfo, State)
generate (State oldGen) Config {..} =
  ( I.SecretTokenInfo
      { stiToken = I.SecretToken tokenBytes
      , stiHash = hashWithAlgorithm cfHashAlgorithm tokenBytes
      , stiHashAlgorithm = cfHashAlgorithm
      }
  , State newGen)
  where
    (tokenBytes, newGen) =
      Random.withDRG oldGen $ Random.getRandomBytes cfTokenLength

hashWithAlgorithm :: I.HashAlgorithm -> BS.ByteString -> BS.ByteString
hashWithAlgorithm I.HashAlgorithmSHA256 bytes =
  BA.convert $ Hash.hashWith Hash.SHA256 bytes

tokenMatchesHash :: I.SecretTokenInfo -> Bool
tokenMatchesHash I.SecretTokenInfo {..} =
  hashWithAlgorithm stiHashAlgorithm (I.secretTokenBytes stiToken) == stiHash
