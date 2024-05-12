{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Helper.GitHub (
  Assignment,
  Classroom,
  AcceptedAssignment,
  Review,
  get,
) where

import Data.Aeson.Schema (Object, schema, get)

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

