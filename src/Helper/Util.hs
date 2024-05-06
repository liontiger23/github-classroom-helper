{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Helper.Util (
  safeReadFile,
  parseAndExtractPointsFromSvg,
) where

import Control.Arrow (left)
import Control.Exception (IOException)
import Control.Exception.Base (catch)
import Data.ByteString.Lazy qualified as B
import Data.Text.Read (decimal)
import Text.XML (Document, def, parseLBS)
import Text.XML.Cursor (content, fromDocument, laxElement, ($//), (&/))

safeReadFile :: FilePath -> IO (Maybe B.ByteString)
safeReadFile f = (Just <$> B.readFile f) `catch` handler
  where
    handler :: IOException -> IO (Maybe B.ByteString)
    handler _ = pure Nothing

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
