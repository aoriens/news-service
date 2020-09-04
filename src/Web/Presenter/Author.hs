module Web.Presenter.Author
  ( presentAuthor
  , RB.Handle
  ) where

import qualified Core.Author as Core
import qualified Data.ByteString.Builder as BB
import Web.Representation.Author
import qualified Web.RepresentationBuilder as RB

presentAuthor :: RB.Handle -> Core.Author -> BB.Builder
presentAuthor h = RB.runBuilder h . authorRepresentation
