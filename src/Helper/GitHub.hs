{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Helper.GitHub (
  Assignment,
  Classroom,
  AcceptedAssignment,
  get,
) where

import Data.Aeson.Schema (Object, schema, get)

type User = Object [schema|
  {
    id: Int,
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
    students: List User,
    repository: Repo,
    assignment: Assignment,
  }
|]

