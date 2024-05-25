{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Helper.GitHub (
  readGHSettings,
  runGH,
  mapGHConcurrently,
  withGHSettings,
) where

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
