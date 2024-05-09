{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Helper.GitHub (
  Assignment (..),
  Classroom (..),
  classrooms,
  classroomAssignments,
  renderClassrooms,
  renderAssignments,
) where

import Text.PrettyPrint.Boxes (hcat, printBox, vcat, left, text, hsep, render)
import Data.Aeson (
  FromJSON,
  ToJSON (toJSON),
  object,
  withObject,
  (.:),
  (.=),
 )
import Data.Aeson.Schema ( Object, schema )
import Data.Aeson.Types (parseJSON)
import Data.ByteString.Char8 (strip)
import Data.ByteString.Lazy qualified as B
import Data.Text (Text)
import GitHub.REST (
  GitHubSettings (..),
  KeyValue ((:=)),
  MonadGitHubREST (..),
  StdMethod (GET),
 )
import GitHub.REST.Auth (Token (..))
import GitHub.REST.Endpoint (GHEndpoint (..))
import Data.List (transpose)
import Helper.Util (renderTable)

data Assignment = Assignment
  { assignmentId :: Int
  , assignmentTitle :: String
  , assignmentInviteLink :: String
  , assignmentSlug :: String
  }
  deriving (Show)

instance ToJSON Assignment where
  toJSON (Assignment aid atitle alink aslug) =
    object
      [ "id"          .= aid
      , "title"       .= atitle
      , "invite_link" .= alink
      , "slug"        .= aslug
      ]

instance FromJSON Assignment where
  parseJSON = withObject "Assignment" $ \obj ->
    Assignment
      <$> obj .: "id"
      <*> obj .: "title"
      <*> obj .: "invite_link"
      <*> obj .: "slug"

renderAssignments :: [Assignment] -> String
renderAssignments = renderTable ["ID", "Title", "Slug", "Invite link"] . map row
  where row (Assignment aid atitle alink aslug) = [show aid, atitle, aslug, alink]

data Classroom = Classroom
  { classroomId :: Int
  , classroomName :: String
  , classroomArchived :: Bool
  , classroomUrl :: String
  }
  deriving (Show)

instance ToJSON Classroom where
  toJSON (Classroom cid cname carchived curl) =
    object
      [ "id"       .= cid
      , "name"     .= cname
      , "archived" .= carchived
      , "url"      .= curl
      ]

instance FromJSON Classroom where
  parseJSON = withObject "Classroom" $ \obj ->
    Classroom
      <$> obj .: "id"
      <*> obj .: "name"
      <*> obj .: "archived"
      <*> obj .: "url"

renderClassrooms :: [Classroom] -> String
renderClassrooms = renderTable ["ID", "Name", "URL"] . map row
  where row (Classroom cid cname _ curl) = [show cid, cname, curl]

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



getToken = AccessToken . strip . B.toStrict <$> B.readFile ".github_token"

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
          , "repo"  := repo
          , "path"  := path <> refStr
          ]
      , ghData = []
      }
 where
  refStr = case ref of
    Just r -> "?ref=" <> r
    Nothing -> ""
