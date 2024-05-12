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
  runGH,
 )
import Helper.Util (renderTable)
import Options.Applicative hiding (action)

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
 <> command "accepted" (info (acceptedAssignmentsCommand <$> argument auto (metavar "ASSIGNMENT_ID")) $
    progDesc "List accepted assignments of assignment with specified ASSIGNMENT_ID") )

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
acceptedAssignmentsCommand aid = runGH (acceptedAssignments aid)
  >>= putStr . renderTable ["ID", "Submitted", "Passing", "Grade", "Students", "Repo"] row
 where
  row :: AcceptedAssignment -> [String]
  row x =
    [ show [get| x.id |]
    , show [get| x.submitted |]
    , show [get| x.passing |]
    , fromMaybe "" [get| x.grade |]
    , unwords [get| x.students[].login |]
    , "https://github.com/" ++ [get| x.repository.full_name |]
    ]
