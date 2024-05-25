{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Helper.GitHub.Endpoint (
  classrooms,
  classroomAssignments,
  acceptedAssignments,
  pullRequestReviews,
  contentsEndpoint,
) where

import GitHub.REST (
  KeyValue ((:=)),
  MonadGitHubREST (..),
  StdMethod (GET),
 )
import GitHub.REST.Endpoint (GHEndpoint (..))
import Helper.GitHub.Schemas
import Data.Aeson.Schema (Object, schema)
import Data.Text (Text)

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

