{-# LANGUAGE TemplateHaskell #-}

module Web.Representation.Author
  ( AuthorRep
  , authorRep
  ) where

import Core.Author
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import Data.Int
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Web.Representation.User
import Web.RepresentationBuilder

data AuthorRep =
  AuthorRep
    { authorAuthorId :: Int32
    , authorUser :: UserRep
    , authorDescription :: T.Text
    }

authorRep :: Author -> RepBuilder AuthorRep
authorRep Author {..} = do
  userR <- userRep Nothing authorUser
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
