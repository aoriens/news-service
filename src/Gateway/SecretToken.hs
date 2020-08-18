{-# LANGUAGE RecordWildCards #-}

module Gateway.SecretToken
  ( generate
  , generateIO
  , initState
  , initIOState
  , tokenMatchesHash
  , State
  , Config(..)
  , IOState
  ) where

import Control.Concurrent.MVar
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

newtype IOState =
  IOState (MVar State)

initState :: IO State
initState = State <$> Random.drgNew

initIOState :: IO IOState
initIOState = IOState <$> (initState >>= newMVar)

generate :: Config -> State -> (State, I.SecretTokenInfo)
generate Config {..} (State oldGen) =
  ( State newGen
  , I.SecretTokenInfo
      { stiToken = I.SecretToken tokenBytes
      , stiHash = hashWithAlgorithm cfHashAlgorithm tokenBytes
      , stiHashAlgorithm = cfHashAlgorithm
      })
  where
    (tokenBytes, newGen) =
      Random.withDRG oldGen $ Random.getRandomBytes cfTokenLength

generateIO :: Config -> IOState -> IO I.SecretTokenInfo
generateIO config (IOState mvar) = modifyMVar mvar (pure . generate config)

hashWithAlgorithm :: I.HashAlgorithm -> BS.ByteString -> BS.ByteString
hashWithAlgorithm I.HashAlgorithmSHA256 bytes =
  BA.convert $ Hash.hashWith Hash.SHA256 bytes

tokenMatchesHash :: I.SecretTokenInfo -> Bool
tokenMatchesHash I.SecretTokenInfo {..} =
  hashWithAlgorithm stiHashAlgorithm (I.secretTokenBytes stiToken) == stiHash
