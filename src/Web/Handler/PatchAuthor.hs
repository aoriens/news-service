{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Web.Handler.PatchAuthor
  ( run
  , Handle(..)
  ) where

import Core.Author
import qualified Core.Interactor.UpdateAuthor as I
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Network.Wai as Wai
import Web.Credentials

data Handle =
  Handle
    { hUpdateAuthorHandle :: I.Handle IO
    , hLoadJSONRequestBody :: forall a. A.FromJSON a =>
                                          Wai.Request -> IO a
    , hPresenter :: Author -> Wai.Response
    }

run :: Handle -> AuthorId -> Wai.Application
run Handle {..} authorIdent request respond = do
  creds <- getCredentialsFromRequest request
  InAuthor {inDescription} <- hLoadJSONRequestBody request
  newAuthor <- I.run hUpdateAuthorHandle creds authorIdent inDescription
  respond $ hPresenter newAuthor

newtype InAuthor =
  InAuthor
    { inDescription :: T.Text
    }

$(A.deriveFromJSON
    A.defaultOptions
      {A.fieldLabelModifier = A.camelTo2 '_' . fromJust . stripPrefix "in"}
    ''InAuthor)
