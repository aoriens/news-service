module Data.Text.Show
  ( showAsText
  ) where

import qualified Data.Text as T

showAsText :: Show a => a -> T.Text
showAsText = T.pack . show
