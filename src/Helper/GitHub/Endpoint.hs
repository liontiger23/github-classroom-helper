{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Helper.GitHub.Endpoint (
  runGH,
  classrooms,
  classroomAssignments,
  acceptedAssignments,
  pullRequestReviews,
  contentsEndpoint,
) where

import Data.ByteString.Char8 (strip)
import Data.ByteString.Lazy qualified as B
import GitHub.REST (
  GitHubSettings (..),
  KeyValue ((:=)),
  MonadGitHubREST (..),
  StdMethod (GET),
  runGitHubT,
 )
import GitHub.REST.Auth (Token (..))
import GitHub.REST.Endpoint (GHEndpoint (..))
import GitHub.REST.Monad (GitHubT)
import Helper.GitHub
import Helper.Util (safeReadFile)
import Data.Aeson.Schema (Object, schema)
import Data.Text (Text)

runGH :: GitHubT IO a -> IO a
runGH action = do
  tokenString <- safeReadFile ".github_token"
  let settings =
        GitHubSettings
          { token = AccessToken . strip . B.toStrict <$> tokenString
          , userAgent = ""
          , apiVersion = "2022-11-28"
          }
  runGitHubT settings action

classrooms :: (MonadGitHubREST m) => m [Classroom]
classrooms =
  queryGitHub
    GHEndpoint
      { method = GET
      , endpoint = "/classrooms"
      , endpointVals = []
      , ghData = []
      }

classroomAssignments :: (MonadGitHubREST m) => Int -> m [Assignment]
classroomAssignments classroom =
  queryGitHub
    GHEndpoint
      { method = GET
      , endpoint = "/classrooms/:classroom/assignments"
      , endpointVals =
          [ "classroom" := classroom
          ]
      , ghData = []
      }

acceptedAssignments :: (MonadGitHubREST m) => Int -> m [AcceptedAssignment]
acceptedAssignments assignment =
  queryGitHub
    GHEndpoint
      { method = GET
      , endpoint = "/assignments/:assignment/accepted_assignments"
      , endpointVals =
          [ "assignment" := assignment
          ]
      , ghData = []
      }

pullRequestReviews :: (MonadGitHubREST m) => String -> String -> Int -> m [Review]
pullRequestReviews owner repo pr =
  queryGitHub
    GHEndpoint
      { method = GET
      , endpoint = "/repos/:owner/:repo/pulls/:pr/reviews"
      , endpointVals =
          [ "owner" := owner
          , "repo" := repo
          , "pr" := pr
          ]
      , ghData = []
      }

type ContentsSchema = Object [schema|
  {
    content: Text,
  }
|]

contentsEndpoint :: (MonadGitHubREST m) => Text -> Text -> Text -> Maybe Text -> m ContentsSchema
contentsEndpoint owner repo path ref =
  queryGitHub
    GHEndpoint
      { method = GET
      , endpoint = "/repos/:owner/:repo/contents/:path"
      , endpointVals =
          [ "owner" := owner
          , "repo"  := repo
          , "path"  := path <> refStr
          ]
      , ghData = []
      }
 where
  refStr = case ref of
    Just r -> "?ref=" <> r
    Nothing -> ""

