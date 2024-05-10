{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Helper.GitHub (
  Assignment (..),
  Classroom (..),
  AcceptedAssignment (..),
) where

import Data.Aeson (
  FromJSON,
  ToJSON (toJSON),
  object,
  withObject,
  (.:),
  (.=),
 )
import Data.Aeson.Types (parseJSON)
import Helper.Renderer (Rendered (..), Header (..))
import Data.Maybe (fromMaybe)

data User = User
  { userId :: Int
  , userLogin :: String
  }
  deriving (Show)

instance ToJSON User where
  toJSON (User uid ulogin) =
    object
      [ "id"       .= uid
      , "login"    .= ulogin
      ]

instance FromJSON User where
  parseJSON = withObject "User" $ \obj ->
    User
      <$> obj .: "id"
      <*> obj .: "login"

data Repo = Repo
  { repoId :: Int
  , repoFullName :: String
  , repoPrivate :: Bool
  , repoDefaultBranch :: String
  }
  deriving (Show)

instance ToJSON Repo where
  toJSON (Repo rid rname rprivate rbranch) =
    object
      [ "id"             .= rid
      , "full_name"      .= rname
      , "private"        .= rprivate
      , "default_branch" .= rbranch
      ]

instance FromJSON Repo where
  parseJSON = withObject "Repo" $ \obj ->
    Repo
      <$> obj .: "id"
      <*> obj .: "full_name"
      <*> obj .: "private"
      <*> obj .: "default_branch"

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

instance Rendered Classroom where
  header = Header ["ID", "Name", "URL"]
  row (Classroom cid cname _ curl) = [show cid, cname, curl]

data Assignment = Assignment
  { assignmentId :: Int
  , assignmentTitle :: String
  , assignmentInviteLink :: String
  , assignmentSlug :: String
  , assignmentClassroom :: Classroom
  }
  deriving (Show)

instance ToJSON Assignment where
  toJSON (Assignment aid atitle alink aslug aclassroom) =
    object
      [ "id"          .= aid
      , "title"       .= atitle
      , "invite_link" .= alink
      , "slug"        .= aslug
      , "classroom"   .= aclassroom
      ]

instance FromJSON Assignment where
  parseJSON = withObject "Assignment" $ \obj ->
    Assignment
      <$> obj .: "id"
      <*> obj .: "title"
      <*> obj .: "invite_link"
      <*> obj .: "slug"
      <*> obj .: "classroom"

instance Rendered Assignment where
  header = Header ["ID", "Title", "Slug", "Invite link"]
  row (Assignment aid atitle alink aslug _) = [show aid, atitle, aslug, alink]

data AcceptedAssignment = AcceptedAssignment
  { acceptedAssignmentId :: Int
  , acceptedAssignmentSubmitted :: Bool
  , acceptedAssignmentPassing :: Bool
  , acceptedAssignmentGrade :: Maybe String
  , acceptedAssignmentStudents :: [User]
  , acceptedAssignmentRepository :: Repo
  , acceptedAssignmentAssignment :: Assignment
  }
  deriving (Show)

instance ToJSON AcceptedAssignment where
  toJSON (AcceptedAssignment aid asubmitted apassing agrade astudents arepo aassignment) =
    object
      [ "id"          .= aid
      , "submitted"   .= asubmitted
      , "passing"     .= apassing
      , "grade"       .= agrade
      , "students"    .= astudents
      , "repository"  .= arepo
      , "assignment"  .= aassignment
      ]

instance FromJSON AcceptedAssignment where
  parseJSON = withObject "AcceptedAssignment" $ \obj ->
    AcceptedAssignment
      <$> obj .: "id"
      <*> obj .: "submitted"
      <*> obj .: "passing"
      <*> obj .: "grade"
      <*> obj .: "students"
      <*> obj .: "repository"
      <*> obj .: "assignment"

instance Rendered AcceptedAssignment where
  header = Header ["ID", "Submitted", "Passing", "Grade", "Students", "Repo"]
  row (AcceptedAssignment aid asubmitted apassing agrade astudents arepo _) =
    [ show aid
    , show asubmitted
    , show apassing
    , fromMaybe "" agrade
    , unwords $ map userLogin astudents
    , "https://github.com/" ++ repoFullName arepo
    ]

