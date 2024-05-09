{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Helper.Util (
  safeReadFile,
  parseAndExtractPointsFromSvg,
  renderTable,
) where

import Control.Arrow (left)
import Control.Exception (IOException)
import Control.Exception.Base (catch)
import Data.ByteString.Lazy qualified as B
import Data.Text.Read (decimal)
import Text.XML (Document, def, parseLBS)
import Text.XML.Cursor (content, fromDocument, laxElement, ($//), (&/))
import Text.PrettyPrint.Boxes (render, hsep, vcat)
import Text.PrettyPrint.Boxes qualified as Boxes
import Data.List (transpose)

safeReadFile :: FilePath -> IO (Maybe B.ByteString)
safeReadFile f = (Just <$> B.readFile f) `catch` handler
  where
    handler :: IOException -> IO (Maybe B.ByteString)
    handler _ = pure Nothing

renderTable :: [String] -> [[String]] -> String
renderTable header table =
  render
    $ hsep 2 Boxes.left
    $ map (vcat Boxes.left . map Boxes.text)
    $ transpose (header : table)

parseAndExtractPointsFromSvg :: B.ByteString -> Either String Int
parseAndExtractPointsFromSvg svg = do
  doc <- left show $ parseLBS def svg
  case extractPointsFromSvg doc of
    [] -> Left "No points extracted"
    [res] -> Right res
    _ -> Left "Too many points extracted"

extractPointsFromSvg :: Document -> [Int]
extractPointsFromSvg doc = do
  text <- fromDocument doc $// laxElement "text" &/ content
  case decimal text of
    Right (res, _) -> [res]
    Left _ -> []
