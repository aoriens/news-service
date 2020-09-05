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
import qualified Core.Authentication as Auth
import qualified Crypto.Hash as Hash
import qualified Crypto.Random as Random
import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
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

generate :: Config -> State -> (State, (Auth.SecretToken, Auth.SecretTokenHash))
generate Config {..} (State oldGen) =
  ( State newGen
  , (Auth.SecretToken tokenBytes, Auth.SecretTokenHash $ hashDefault tokenBytes))
  where
    (tokenBytes, newGen) =
      Random.withDRG oldGen $ Random.getRandomBytes cfTokenLength

generateIO :: Config -> IOState -> IO (Auth.SecretToken, Auth.SecretTokenHash)
generateIO config (IOState mvar) = modifyMVar mvar (pure . generate config)

hashDefault :: B.ByteString -> B.ByteString
hashDefault = hashWith sha256Algorithm

hashWith :: HashAlgorithm -> B.ByteString -> B.ByteString
hashWith HashAlgorithm {..} = B.cons haHashPrefix . haHasher
                              -- See Note [Multiple hash algorithms]

tokenMatchesHash :: Auth.SecretToken -> Auth.SecretTokenHash -> Bool
tokenMatchesHash (Auth.SecretToken token) (Auth.SecretTokenHash hash)
  -- See Note [Multiple hash algorithms]
 =
  case algorithmOfHash hash of
    Nothing -> False
    Just alg -> hashWith alg token == hash

algorithmOfHash :: B.ByteString -> Maybe HashAlgorithm
algorithmOfHash s = do
  (prefix, _) <- B.uncons s
  find ((prefix ==) . haHashPrefix) supportedAlgorithms
  where
    supportedAlgorithms = [sha256Algorithm]

data HashAlgorithm =
  HashAlgorithm
    { haHasher :: Hasher
    , haHashPrefix :: Word8
    }

type Hasher = B.ByteString -> B.ByteString

sha256Algorithm :: HashAlgorithm
sha256Algorithm =
  HashAlgorithm
    {haHasher = BA.convert . Hash.hashWith Hash.SHA256, haHashPrefix = 0}
{-

Note [Multiple hash algorithms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Hash algorithms may get obsolete and be showing vulnerabilities over
time. So, changing a hash algorithm in future should be possible, yet
keeping ability to authenticate credentials hashed with an obsolete
algorithms. It is implemented by prepending a hash with an extra octet
to encode the hash algorithm. When matching a credential against a
hash, the proper algorithm is to be determined out of the first octet.

-}
