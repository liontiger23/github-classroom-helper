{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Helper (main) where

import Control.Monad (join)
import Data.Maybe (fromMaybe)
import Helper.GitHub (AcceptedAssignment, Assignment, Classroom, get)
import Helper.GitHub.Endpoint (
  acceptedAssignments,
  classroomAssignments,
  classrooms,
  pullRequestReviews,
  runGH,
 )
import Helper.Util (renderTable, renderTable_)
import Options.Applicative hiding (action)
import GitHub.REST (MonadGitHubREST)
import Data.List.Split (splitOn)
import GitHub.REST.Monad (GitHubT)
import Text.DocTemplates
import Data.Aeson
import Text.DocLayout (render)

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
    progDesc "List available assignments in classroom with specified CLASSROOM_ID")
 <> command "accepted-all" (info (allAcceptedAssignmentsCommand <$> argument auto (metavar "CLASSROOM_ID")) $
    progDesc "List accepted assignments of assignment with specified CLASSROOM_ID")
 <> command "accepted" (info (acceptedAssignmentsCommand <$> argument auto (metavar "ASSIGNMENT_ID")) $
    progDesc "List accepted assignments of assignment with specified ASSIGNMENT_ID")
 <> command "report" (info ( reportCommand
                         <$> argument auto (metavar "CLASSROOM_ID")
                         <*> argument str  (metavar "TEMPLATE_FILE") ) $
    progDesc "List accepted assignments of assignment with specified CLASSROOM_ID") )

classroomsCommand :: IO ()
classroomsCommand = runGH classrooms
  >>= putStr . renderTable ["ID", "NAME", "URL"] row
 where
   row :: Classroom -> [String]
   row x =
    [ show [get| x.id |]
    , [get| x.name |]
    , [get| x.url |]
    ]

assignmentsCommand :: Int -> IO ()
assignmentsCommand cid = runGH (classroomAssignments cid)
  >>= putStr . renderTable ["ID", "Title", "Slug", "Invite link"] row
 where
  row :: Assignment -> [String]
  row x =
    [ show [get| x.id |]
    , [get| x.title |]
    , [get| x.slug |]
    , [get| x.invite_link |]
    ]

acceptedAssignmentsCommand :: Int -> IO ()
acceptedAssignmentsCommand aid = acceptedAssignmentsCommand' $ acceptedAssignments aid

allAcceptedAssignmentsCommand :: Int -> IO ()
allAcceptedAssignmentsCommand cid = acceptedAssignmentsCommand' $
  classroomAssignments cid >>= \as -> concat <$> mapM (\a -> acceptedAssignments [get| a.id |]) as

acceptedAssignmentsCommand' :: GitHubT IO [AcceptedAssignment] -> IO ()
acceptedAssignmentsCommand' assignmentsM = runGH process
  >>= putStr . renderTable_ ["Students", "Grade", "Review", "Repo"]
 where
  process :: GitHubT IO [[String]]
  process = do
    assignments <- assignmentsM
    reviews <- mapM passedReview assignments
    pure $ do
      (a, r) <- zip assignments reviews
      pure
        [ unwords [get| a.students[].login |]
        , fromMaybe "" [get| a.grade |]
        , if r then "✅" else "❌"
        , "https://github.com/" ++ [get| a.repository.full_name |]
        ]
  passedReview :: (MonadGitHubREST m) => AcceptedAssignment -> m Bool
  passedReview assignment = do
    reviews <- pullRequestReviews owner repo 1
    let r = last reviews
    pure $ not (null reviews) && [get| r.state |] == "APPROVED"
   where
    repoFullName = [get| assignment.repository.full_name |]
    (owner, repo) = case splitOn "/" repoFullName of
      [x, y] -> (x, y)
      _ -> error $ "Unexpected repository name " ++ repoFullName

reportCommand :: Int -> FilePath -> IO ()
reportCommand cid templateFile = do
  -- TODO
  t <- either error id <$> compileTemplateFile templateFile
  putStrLn $ render Nothing $ renderTemplate t $ object
    [ "assignments" .=
      [ object ["title" .= ("foo" :: String)]
      , object ["title" .= ("bar" :: String)]
      ]
    , "students" .=
      [ object
        [ "name" .= ("John" :: String)
        , "results" .= (["0/10", "8/10"] :: [String])
        ]
      ]
    ]
 --where
  -- TODO
  --fetchData :: GitHubT IO ([Assignment], [
