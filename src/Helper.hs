{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Helper (main) where

import Control.Monad (join)
import Data.Aeson (Array, FromJSON, ToJSON, Value)
import Data.Aeson.Schema (Object, get, schema)
import Data.Aeson.Types (Result, fromJSON)
import Data.ByteString.Base64.Lazy (decodeLenient)
import Data.ByteString.Char8 (strip)
import Data.ByteString.Lazy qualified as B
import Data.List (find)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import GitHub.REST
import Helper.GitHub (
  Classroom (classroomId),
  classroomAssignments,
  classrooms,
  renderAssignments,
  renderClassrooms,
 )
import Helper.Util (parseAndExtractPointsFromSvg, safeReadFile)
import Options.Applicative hiding (action)
import Data.Text.Lazy.Encoding (decodeUtf8)

getToken = (AccessToken . strip . B.toStrict <$>) <$> safeReadFile ".github_token"

defaultState token =
  GitHubSettings
    { token = token
    , userAgent = "nsu-syspro"
    , apiVersion = "2022-11-28"
    }


type ContentsSchema =
  [schema|
  {
    content: Text,
  }
|]

contentsEndpoint :: (MonadGitHubREST m) => Text -> Text -> Text -> Maybe Text -> m (Object ContentsSchema)
contentsEndpoint owner repo path ref =
  queryGitHub
    GHEndpoint
      { method = GET
      , endpoint = "/repos/:owner/:repo/contents/:path"
      , endpointVals =
          [ "owner" := owner
          , "repo" := repo
          , "path" := path <> refStr
          ]
      , ghData = []
      }
 where
  refStr = case ref of
    Just r -> "?ref=" <> r
    Nothing -> ""

main' :: IO ()
main' = do
  name <- execParser $ info (argument str (metavar "NAME")) fullDesc
  token <- getToken
  res <-
    runGitHubT (defaultState token) $
      contentsEndpoint
        "nsu-syspro"
        ("mpt-markup-basics-" <> name)
        ".github/badges/points-bar.svg"
        (Just "badges")
  print $ parseAndExtractPointsFromSvg $ decodeLenient $ B.fromStrict $ encodeUtf8 [get| res.content |]

main :: IO ()
main = join $ execParser $ info (helper <*> opts)
  ( fullDesc
 <> progDesc "Provides helper utilities for working with GitHub classroom and student assignments"
 <> header "GitHub Classroom Helper" )

opts :: Parser (IO ())
opts = subparser
  ( command "classrooms" (info (pure classroomsCommand) $
    progDesc "List available classrooms" )
 <> command "assignments" (info (assignmentsCommand <$> argument auto (metavar "CLASSROOM_ID")) $
    progDesc "List available assignments in classroom with specified CLASSROOM_ID") )

runGH :: GitHubT IO a -> IO a
runGH action = do
  token <- getToken
  runGitHubT (defaultState token) action

classroomsCommand :: IO ()
classroomsCommand = do
  classroom <- runGH classrooms
  putStr $ renderClassrooms classroom

assignmentsCommand :: Int -> IO ()
assignmentsCommand cid = do
  assignment <- runGH $ classroomAssignments cid
  putStr $ renderAssignments assignment

