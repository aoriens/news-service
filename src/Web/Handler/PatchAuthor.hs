{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Web.Handler.PatchAuthor
  ( run
  , Handle(..)
  ) where

import qualified Core.Interactor.UpdateAuthor as I
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Network.Wai as Wai
import Web.Credentials
import Web.PathParameter
import Web.Representation.Author
import Web.RepresentationBuilder

data Handle =
  Handle
    { hUpdateAuthorHandle :: I.Handle IO
    , hLoadJSONRequestBody :: forall a. A.FromJSON a =>
                                          Wai.Request -> IO a
    , hPresenterHandle :: RepBuilderHandle
    }

run :: Handle -> Wai.Application
run Handle {..} request respond = do
  creds <- getCredentialsFromRequest request
  authorIdent <- getAuthorIdFromPath $ Wai.pathInfo request
  InAuthor {inDescription} <- hLoadJSONRequestBody request
  newAuthor <- I.run hUpdateAuthorHandle creds authorIdent inDescription
  respond $ runRepBuilder hPresenterHandle $ authorRepresentation newAuthor

newtype InAuthor =
  InAuthor
    { inDescription :: T.Text
    }

$(A.deriveFromJSON
    A.defaultOptions
      {A.fieldLabelModifier = A.camelTo2 '_' . fromJust . stripPrefix "in"}
    ''InAuthor)
