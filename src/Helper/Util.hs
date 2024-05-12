{-# LANGUAGE ImportQualifiedPost #-}

module Helper.Util (
  safeReadFile,
  renderTable,
  renderTable_,
) where

import Control.Exception (IOException)
import Control.Exception.Base (catch)
import Data.ByteString.Lazy qualified as B
import Data.List (transpose)
import Text.PrettyPrint.Boxes (
  hsep,
  render,
  vcat,
 )
import Text.PrettyPrint.Boxes qualified as Boxes

safeReadFile :: FilePath -> IO (Maybe B.ByteString)
safeReadFile f = (Just <$> B.readFile f) `catch` handler
  where
    handler :: IOException -> IO (Maybe B.ByteString)
    handler _ = pure Nothing

renderTable :: [String] -> (a -> [String]) -> [a] -> String
renderTable header row xs = renderTable_ (header : map row xs)

renderTable_ :: [[String]] -> String
renderTable_ table =
  render
    $ hsep 2 Boxes.left
    $ map (vcat Boxes.left . map Boxes.text)
    $ transpose table
