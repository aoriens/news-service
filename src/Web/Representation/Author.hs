{-# LANGUAGE TemplateHaskell #-}

module Web.Representation.Author
  ( Author
  , authorRepresentation
  ) where

import qualified Core.Author as Core
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import Data.Int
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Web.Representation.User
import qualified Web.RepresentationBuilder as RB

data Author =
  Author
    { authorAuthorId :: Int32
    , authorUser :: User
    , authorDescription :: T.Text
    }

authorRepresentation :: Core.Author -> RB.Builder Author
authorRepresentation Core.Author {..} = do
  userR <- userRepresentation Nothing authorUser
  pure
    Author
      { authorAuthorId = Core.getAuthorId authorId
      , authorUser = userR
      , authorDescription = authorDescription
      }

$(A.deriveToJSON
    A.defaultOptions
      { A.fieldLabelModifier = A.camelTo2 '_' . fromJust . stripPrefix "author"
      , A.omitNothingFields = True
      }
    ''Author)
