module Helper.Renderer (
  Header (..),
  Rendered (..),
  renderTable
) where

import Text.PrettyPrint.Boxes (render, hsep, vcat, left, text)
import Data.List (transpose)

newtype Header a = Header [String]

class Rendered a where
  header :: Header a
  row :: a -> [String]

renderTable :: (Rendered a) => [a] -> String
renderTable = renderTable'' header

renderTable'' :: (Rendered a) => Header a -> [a] -> String
renderTable'' (Header hs) xs =
  render
    $ hsep 2 left
    $ map (vcat left . map text)
    $ transpose (hs : map row xs)
