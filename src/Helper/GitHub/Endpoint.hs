{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Helper.GitHub.Endpoint (
  runGH,
  classrooms,
  classroomAssignments,
  acceptedAssignments,
  pullRequestReviews,
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
