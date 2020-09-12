module Web.Presenter.Author
  ( presentAuthor
  , presentAuthors
  , RepBuilderHandle
  ) where

import qualified Core.Author as Core
import qualified Network.Wai as Wai
import Web.Representation.Author
import Web.RepresentationBuilder

presentAuthor :: RepBuilderHandle -> Core.Author -> Wai.Response
presentAuthor h = runRepBuilder h . authorRepresentation

presentAuthors :: RepBuilderHandle -> [Core.Author] -> Wai.Response
presentAuthors h = runRepBuilder h . mapM authorRepresentation
