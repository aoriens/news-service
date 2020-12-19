{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Web.Representation.Category
  ( CategoryRep
  , categoryRep
  , optCategoryRep
  ) where

import Control.Arrow
import Core.Category
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A
import Data.Int
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Web.RepresentationBuilder

type CategoryRep = [CategoryItemRep]

data CategoryItemRep =
  CategoryItemRep
    { itemCategoryId :: Int32
    , itemName :: T.Text
    }

optCategoryRep :: Maybe Category -> RepBuilder CategoryRep
optCategoryRep = maybe (pure []) categoryRep

categoryRep :: Category -> RepBuilder CategoryRep
categoryRep = pure . reverse . unfoldr f . Just
  where
    f = fmap (makeItem &&& categoryParent)
    makeItem Category {..} =
      CategoryItemRep
        {itemCategoryId = getCategoryId categoryId, itemName = categoryName}

$(A.deriveToJSON
    A.defaultOptions
      { A.fieldLabelModifier = A.camelTo2 '_' . fromJust . stripPrefix "item"
      , A.omitNothingFields = True
      }
    ''CategoryItemRep)
