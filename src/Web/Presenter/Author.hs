module Web.Presenter.Author
  ( presentAuthor
  , presentAuthors
  , RepBuilderHandle
  ) where

import qualified Core.Author as Core
import qualified Data.ByteString.Builder as BB
import Web.Representation.Author
import Web.RepresentationBuilder

presentAuthor :: RepBuilderHandle -> Core.Author -> BB.Builder
presentAuthor h = runRepBuilder h . authorRepresentation

presentAuthors :: RepBuilderHandle -> [Core.Author] -> BB.Builder
presentAuthors h = runRepBuilder h . mapM authorRepresentation
