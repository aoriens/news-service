{-# LANGUAGE RankNTypes #-}

module Web.Presenter.UserPresenter
  ( presentUser
  , presentUsers
  , Handle(..)
  ) where

import qualified Core.Authentication as Auth
import qualified Core.User as C
import qualified Data.Aeson as A
import qualified Data.ByteString.Builder as BB
import Data.Text (Text)
import qualified Web.AppURL as U
import Web.Entity.User

data Handle =
  Handle
    { hJSONEncode :: forall a. A.ToJSON a =>
                                 a -> BB.Builder
    , hRenderAppURL :: U.AppURL -> Text
    }

presentUser :: Handle -> C.User -> Maybe Auth.Credentials -> BB.Builder
presentUser h user creds =
  hJSONEncode h $ userEntity (hRenderAppURL h) creds user

presentUsers :: Handle -> [C.User] -> BB.Builder
presentUsers h = hJSONEncode h . map (userEntity (hRenderAppURL h) Nothing)
