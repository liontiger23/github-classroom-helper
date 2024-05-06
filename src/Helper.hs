{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Helper (main) where

import Control.Arrow (left)
import Data.Aeson (Array, FromJSON, ToJSON, Value, decode)
import Data.Aeson.Schema
import Data.Aeson.Types (Result, fromJSON)
import Data.ByteString.Base64.Lazy (decodeLenient)
import Data.ByteString.Char8 (strip)
import Data.ByteString.Lazy qualified as B
import Data.Char (ord)
import Data.List (find)
import Data.Maybe (listToMaybe, maybeToList)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Read (decimal)
import GHC.Generics (Generic)
import GitHub.REST
import Helper.GitHub
import Text.XML (Document, def, parseLBS, parseLBS_)
import Text.XML.Cursor (
  child,
  content,
  fromDocument,
  laxElement,
  ($//),
  (&/),
 )
import Control.Exception (IOException)
import Control.Exception.Base (catch)
import Helper.Util (safeReadFile, parseAndExtractPointsFromSvg)
import Options.Applicative


getToken = (AccessToken . strip . B.toStrict <$>) <$> safeReadFile ".github_token"

defaultState token =
  GitHubSettings
    { token = token
    , userAgent = "nsu-syspro"
    , apiVersion = "2022-11-28"
    }

myClassroomId = 199096

myClassroom cid = do
  token <- getToken
  cs <- runGitHubT (defaultState token) classrooms
  return $ find ((== cid) . classroomId) cs

testAssignments = do
  token <- getToken
  Just classroom <- myClassroom myClassroomId
  arr <- runGitHubT (defaultState token) (classroomAssignments $ classroomId classroom)
  print arr
  print ""

type ClassroomSchema =
  [schema|
  {
    id: Int,
    name: String,
    archived: Bool,
    url: String
  }
|]

data Classroom = Classroom
  { id :: Int
  , name :: String
  , archived :: Bool
  , url :: String
  }
  deriving (Show, Generic, FromJSON, ToJSON)

asClassroom :: Value -> Result (Object ClassroomSchema)
asClassroom = fromJSON

testClassrooms = do
  token <- getToken
  arr <- runGitHubT (defaultState token) classroomsEndpoint :: IO Array
  mapM_ (mapM_ (\x -> print [get| x.name|])) $ mapM asClassroom arr

classroomsEndpoint :: (MonadGitHubREST m, FromJSON a) => m a
classroomsEndpoint =
  queryGitHub
    GHEndpoint
      { method = GET
      , endpoint = "/classrooms"
      , endpointVals = []
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
          , "repo" := repo
          , "path" := path <> refStr
          ]
      , ghData = []
      }
 where
  refStr = case ref of
    Just r -> "?ref=" <> r
    Nothing -> ""

main :: IO ()
main = do
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
