{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Helper.GitHub (
  Assignment,
  Classroom,
  AcceptedAssignment,
  Review,
  User,
  get,
  readGHSettings,
  runGH,
  mapGHConcurrently,
  withGHSettings,
) where

import Data.Aeson.Schema (Object, schema, get)
import Data.ByteString.Char8 (strip)
import Data.ByteString.Lazy qualified as B
import GitHub.REST (
  GitHubSettings (..), runGitHubT,
 )
import GitHub.REST.Auth (Token (..))
import Helper.Util (safeReadFile)
import GitHub.REST.Monad (GitHubT)
import Control.Monad.Reader
import Control.Concurrent.Async (mapConcurrently)


type User = Object [schema|
  {
    id: Int,
    login: String,
    name: String,
    avatar_url: String,
    html_url: String,
  }
|]

type UserLogin = Object [schema|
  {
    login: String,
  }
|]

type Repo = Object [schema|
  {
    id: Int,
    full_name: String,
    private: Bool,
    default_branch: String,
  }
|]

type Classroom = Object [schema|
  {
    id: Int,
    name: String,
    archived: Bool,
    url: String,
  }
|]

type Assignment = Object [schema|
  {
    id: Int,
    title: String,
    invite_link: String,
    slug: String,
    classroom: Classroom,
  }
|]

type AcceptedAssignment = Object [schema|
  {
    id: Int,
    submitted: Bool,
    passing: Bool,
    grade: Maybe String,
    students: List UserLogin,
    repository: Repo,
    assignment: Assignment,
  }
|]

type Review = Object [schema|
  {
    id: Int,
    user: UserLogin,
    body: String,
    state: String,
  }
|]

mapGHConcurrently :: (Traversable t) => (a -> GitHubT IO b) -> t a -> ReaderT GitHubSettings IO (t b)
mapGHConcurrently action xs = do
  settings <- ask
  lift $ mapConcurrently (runGitHubT settings . action) xs

runGH :: MonadIO m => GitHubT m a -> ReaderT GitHubSettings m a
runGH action = do
  settings <- ask
  lift $ runGitHubT settings action

withGHSettings :: ReaderT GitHubSettings IO a -> IO a
withGHSettings action = readGHSettings >>= runReaderT action

readGHSettings :: IO GitHubSettings
readGHSettings = do
  tokenString <- safeReadFile ".github_token"
  pure GitHubSettings
    { token = AccessToken . strip . B.toStrict <$> tokenString
    , userAgent = ""
    , apiVersion = "2022-11-28"
    }
