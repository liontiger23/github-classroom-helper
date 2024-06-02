{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Helper.Util (
  safeReadFile,
  renderTable,
  renderTable_,
  parseAndExtractPointsFromSvg,
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
import Text.XML
import Control.Arrow (left)
import Text.XML.Cursor
import Data.Text (unpack)

safeReadFile :: FilePath -> IO (Maybe B.ByteString)
safeReadFile f = (Just <$> B.readFile f) `catch` handler
  where
    handler :: IOException -> IO (Maybe B.ByteString)
    handler _ = pure Nothing

-- | Render table with 'header' and rows of 'xs' mapped by 'row' function.
--
-- Convenience wrapper around 'renderTable_'.
renderTable :: [String] -> (a -> [String]) -> [a] -> String
renderTable header row xs = renderTable_ header $ map row xs

-- | Render aligned table with 'header' and 'rows'.
renderTable_ :: [String] -> [[String]] -> String
renderTable_ header rows =
  render
    $ hsep 2 Boxes.left
    $ map (vcat Boxes.left . map Boxes.text)
    $ transpose (header : rows)

parseAndExtractPointsFromSvg :: B.ByteString -> Either String String
parseAndExtractPointsFromSvg svg = do
  doc <- left show $ parseLBS def svg
  case extractTextContentsFromSvg doc of
    -- hard-coded extraction of second text contents
    (_:x:_) -> Right x
    xs -> Left $ "Unexpected content extracted: " ++ show xs

extractTextContentsFromSvg :: Document -> [String]
extractTextContentsFromSvg doc = do
   t <- fromDocument doc $// laxElement "text" &/ content
   pure $ unpack t
