{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Helper (main) where

import Control.Monad (join)
import Data.Maybe (fromMaybe)
import Helper.GitHub (AcceptedAssignment, Assignment, Classroom, get, User)
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
import Data.Map (Map)
import Data.Map qualified as Map
import Data.List (nub)

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
  classroomAssignments cid >>= \as -> concat <$> mapM (acceptedAssignments . [get| .id |]) as

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

reportCommand :: Int -> FilePath -> IO ()
reportCommand cid templateFile = do
  (assignments, users, AssignmentReport reportData) <- runGH fetchData
  t <- either error id <$> compileTemplateFile templateFile
  putStrLn $ render Nothing $ renderTemplate t $ object
    [ "assignments" .= assignments
    , "students" .=
      let
        result :: User -> Assignment -> Maybe Value
        result u a = fmap fromStatus (Map.lookup key reportData)
         where key = [get| a.slug |] ++ "-" ++ [get| u.login |]
        fromStatus :: AssignmentStatus -> Value
        fromStatus (AssignmentStatus g r) = object
          [ "grade"  .= g
          , "review" .= (if r then "✅" else "❌" :: String)
          ]
        student :: User -> Value
        student u = object
            [ "name"    .= [get| u.name |]
            , "login"   .= [get| u.login |]
            , "results" .= map (result u) assignments
            ]
      in map student users
    ]
 where
  fetchData :: GitHubT IO ([Assignment], [User], AssignmentReport)
  fetchData = do
    assignments <- classroomAssignments cid
    accepted <- concat <$> mapM (acceptedAssignments . [get| .id |]) assignments
    let users = nub $ map fakeUser $ concatMap [get| .students[].login |] accepted
    report <- assignmentReport accepted
    pure (assignments, users, report)
  fakeUser :: String -> User
  fakeUser login = case fromJSON val of
    Data.Aeson.Success x -> x
    Data.Aeson.Error e -> error e
   where
    val = object
      [ "id"     .= (42 :: Int)
      , "login" .= login
      , "name"  .= login
      , "avatar_url" .= ("" :: String)
      , "html_url" .= ("" :: String)
      ]


assignmentReport :: (MonadGitHubREST m) => [AcceptedAssignment] -> m AssignmentReport
assignmentReport xs = AssignmentReport . Map.fromList <$> mapM toMapPair xs
 where
  toMapPair :: (MonadGitHubREST m) => AcceptedAssignment -> m (RepoName, AssignmentStatus)
  toMapPair a = (snd $ assignmentRepoParts a,) <$> assignmentStatus a

assignmentStatus :: (MonadGitHubREST m) => AcceptedAssignment -> m AssignmentStatus
assignmentStatus assignment = AssignmentStatus grade <$> passedReview assignment
 where grade = fromMaybe "" [get| assignment.grade |]

passedReview :: (MonadGitHubREST m) => AcceptedAssignment -> m Bool
passedReview assignment = do
  reviews <- pullRequestReviews owner repo 1
  let r = last reviews
  pure $ not (null reviews) && [get| r.state |] == "APPROVED"
 where
  (owner, repo) = assignmentRepoParts assignment

assignmentRepoParts :: AcceptedAssignment -> (OwnerName, RepoName)
assignmentRepoParts a = repoParts [get| a.repository.full_name |]

repoParts :: String -> (OwnerName, RepoName)
repoParts name = case splitOn "/" name of
  [x, y] -> (x, y)
  _ -> error $ "Unexpected repository name " ++ name

type OwnerName = String
type RepoName = String
type Grade = String
data AssignmentStatus = AssignmentStatus Grade Bool
  deriving (Eq, Show)
newtype AssignmentReport = AssignmentReport (Map RepoName AssignmentStatus)

