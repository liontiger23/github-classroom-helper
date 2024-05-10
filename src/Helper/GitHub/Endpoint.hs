{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Helper.GitHub.Endpoint (
  runGH,
  classrooms,
  classroomAssignments,
  acceptedAssignments,
  contentsEndpoint,
) where

import Data.Aeson.Schema ( Object, schema )
import GitHub.REST.Endpoint (GHEndpoint (..))
import GitHub.REST (MonadGitHubREST, StdMethod (GET), KeyValue ((:=)))
import Helper.GitHub
import GitHub.REST.Monad (MonadGitHubREST(..))
import Data.ByteString.Char8 (strip)
import Data.ByteString.Lazy qualified as B
import Data.Text (Text)
import GitHub.REST
  ( GitHubSettings(..),
    KeyValue((:=)),
    MonadGitHubREST(..),
    StdMethod(GET),
    runGitHubT )
import GitHub.REST.Auth (Token (..))
import GitHub.REST.Endpoint (GHEndpoint (..))
import Data.List (transpose)
import Helper.Util (renderTable, safeReadFile)
import GitHub.REST.Monad (GitHubT)

runGH :: GitHubT IO a -> IO a
runGH action = do
  token <- getToken
  runGitHubT (defaultState token) action

getToken = (AccessToken . strip . B.toStrict <$>) <$> safeReadFile ".github_token"


defaultState token =
  GitHubSettings
    { token = token
    , userAgent = "nsu-syspro"
    , apiVersion = "2022-11-28"
    }

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
          , "repo"  := repo
          , "path"  := path <> refStr
          ]
      , ghData = []
      }
 where
  refStr = case ref of
    Just r -> "?ref=" <> r
    Nothing -> ""
