{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Helper (main) where

import Control.Monad (join)
import Data.Aeson.Schema (get)
import Data.ByteString.Base64.Lazy (decodeLenient)
import Data.ByteString.Lazy qualified as B
import Data.Text.Encoding (encodeUtf8)
import Helper.GitHub.Endpoint (
  runGH,
  classroomAssignments,
  classrooms, contentsEndpoint, acceptedAssignments,
 )
import Helper.Util (parseAndExtractPointsFromSvg)
import Options.Applicative hiding (action)
import Helper.Renderer (renderTable)

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
  >>= putStr . renderTable

assignmentsCommand :: Int -> IO ()
assignmentsCommand cid = runGH (classroomAssignments cid)
  >>= putStr . renderTable

acceptedAssignmentsCommand :: Int -> IO ()
acceptedAssignmentsCommand aid = runGH (acceptedAssignments aid)
  >>= putStr . renderTable

main' :: IO ()
main' = do
  name <- execParser $ info (argument str (metavar "NAME")) fullDesc
  res <-
    runGH $
      contentsEndpoint
        "nsu-syspro"
        ("mpt-markup-basics-" <> name)
        ".github/badges/points-bar.svg"
        (Just "badges")
  print $ parseAndExtractPointsFromSvg $ decodeLenient $ B.fromStrict $ encodeUtf8 [get| res.content |]
