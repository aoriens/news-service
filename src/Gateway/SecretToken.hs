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
import Data.List
import Data.Word

newtype Config =
  Config
    { cfTokenLength :: Int
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

generate :: Config -> State -> (State, (I.SecretToken, I.SecretTokenHash))
generate Config {..} (State oldGen) =
  ( State newGen
  , (I.SecretToken tokenBytes, I.SecretTokenHash $ hashDefault tokenBytes))
  where
    (tokenBytes, newGen) =
      Random.withDRG oldGen $ Random.getRandomBytes cfTokenLength

generateIO :: Config -> IOState -> IO (I.SecretToken, I.SecretTokenHash)
generateIO config (IOState mvar) = modifyMVar mvar (pure . generate config)

hashDefault :: BS.ByteString -> BS.ByteString
hashDefault = hashWith sha256Algorithm

hashWith :: HashAlgorithm -> BS.ByteString -> BS.ByteString
hashWith HashAlgorithm {..} = BS.cons haHashPrefix . haHasher
                              -- See Note [Multiple hash algorithms]

tokenMatchesHash :: I.SecretToken -> I.SecretTokenHash -> Bool
tokenMatchesHash (I.SecretToken token) (I.SecretTokenHash hash)
  -- See Note [Multiple hash algorithms]
 =
  case algorithmOfHash hash of
    Nothing -> False
    Just alg -> hashWith alg token == hash

algorithmOfHash :: BS.ByteString -> Maybe HashAlgorithm
algorithmOfHash s = do
  (prefix, _) <- BS.uncons s
  find ((prefix ==) . haHashPrefix) supportedAlgorithms
  where
    supportedAlgorithms = [sha256Algorithm]

data HashAlgorithm =
  HashAlgorithm
    { haHasher :: Hasher
    , haHashPrefix :: Word8
    }

type Hasher = BS.ByteString -> BS.ByteString

sha256Algorithm :: HashAlgorithm
sha256Algorithm =
  HashAlgorithm
    {haHasher = BA.convert . Hash.hashWith Hash.SHA256, haHashPrefix = 0}
{-

Note [Multiple hash algorithms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Hash algorithms may get obsolete and showing vulnerabilities over
time. So, changing a hash algorithm in future should be possible, yet
keeping ability to authenticate credentials hashed with an obsolete
algorithms. It is implemented by prepending a hash with an extra octet
to encode the hash algorithm. When matching a credential against a
hash, the proper algorithm is to be determined out of the first octet.

-}
