{-# LANGUAGE TemplateHaskell #-}

module Web.Representation.Author
  ( AuthorRep
  , authorRep
  ) where

import Core.Author
import Core.Deletable
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import Data.Int
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Web.Representation.OneOf
import Web.Representation.User
import Web.RepresentationBuilder

data AuthorRep =
  AuthorRep
    { authorAuthorId :: Int32
    , authorUser :: OneOfRep T.Text UserRep
    , authorDescription :: T.Text
    }

authorRep :: Author -> RepBuilder AuthorRep
authorRep Author {..} = do
  userR <-
    case authorUser of
      Deleted -> pure $ LeftRep "DELETED"
      Existing user -> RightRep <$> userRep Nothing user
  pure
    AuthorRep
      { authorAuthorId = getAuthorId authorId
      , authorUser = userR
      , authorDescription = authorDescription
      }

$(A.deriveToJSON
    A.defaultOptions
      { A.fieldLabelModifier = A.camelTo2 '_' . fromJust . stripPrefix "author"
      , A.omitNothingFields = True
      }
    ''AuthorRep)
