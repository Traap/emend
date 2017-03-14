-- | Copyright (c) Gary Allan Howard aka Traap.
-- License BSD-3-Clause
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Traap.DataTypes
  (-- Symbolic Links
   Symlinks(..)
  ,Symlink(..)
  ,toSymlinks
  ,toSymlink
  -- Git Repositories
  ,Repos(..)
  ,Repo(..)
  ,Path(..)
  ,toRepos
  ,toRepo
  ,toPath
  -- Programs to install.
  ,Installations(..)
  ,Os(..)
  ,Command(..)
  ,toInstallations
  ,toOs
  ,toCommand
  ) where

import           Data.Yaml
import qualified Data.Text as T
import           GHC.Generics

-- -----------------------------------------------------------------------------
-- Actions bootstrap can perform.
data Action = Create | Delete | Clone | Install

-- -----------------------------------------------------------------------------
-- | The Symlinks type defines the target of a symlink operation and identifies
-- the target type as a file or as a directory.
data Symlinks = SYMLINKS
  { symlinks :: [Symlink]
  } deriving (Show, Generic, FromJSON)

data Symlink = SYMLINK
  { file :: T.Text
  , link :: T.Text
  , directory :: Bool
  } deriving (Show, Generic, FromJSON)

toSymlinks :: Symlinks -> Action -> [T.Text]
toSymlinks s a = case a of
  Create -> map toSymlink (symlinks s)
  Delete -> map toSymlink' (symlinks s)

toSymlink :: Symlink -> T.Text
toSymlink s = mconcat ["lns -s ", file s, " ", link s]

toSymlink' :: Symlink -> T.Text
toSymlink' s = mconcat ["rm -vrf ", link s]

-- -----------------------------------------------------------------------------
-- | The Repos type defines a Repo and whether or not the Repo is cloned to .
-- (here) or a new directory.
data Repos = REPOS
  { repos :: [Repo]
  } deriving (Show, Generic, FromJSON)

data Repo = REPO
  { url :: T.Text
  , paths ::  [Path]
  } deriving (Show, Generic, FromJSON)

data Path = PATH
  { source :: T.Text
  , target :: T.Text
  } deriving (Show, Generic, FromJSON)

toRepos :: Repos -> Action -> [T.Text]
toRepos r a = concatMap (toRepo a) (repos r)

toRepo :: Repo -> Action -> [T.Text]
toRepo REPO {..} a = map (toPath url a) paths

toPath :: T.Text -> Path -> Action -> T.Text
toPath u p a = case a of 
  Clone -> mconcat ["git clone ", u, "/", source p, target p]
  Delete -> mconcat [target p]

-- -----------------------------------------------------------------------------
-- | The Installations type defines a program to run and the argument is is passed.
data Installations = INSTALLATIONS
  { installations :: [Os]
  } deriving (Show, Generic, FromJSON)

data Os = OS
  { name :: T.Text
  , command :: [Command]
  } deriving (Show, Generic, FromJSON)

data Command = COMMAND
  { sudo :: Bool
  , program :: T.Text
  , argument :: T.Text
  } deriving (Show, Generic, FromJSON)

toInstallations :: Installations -> Action -> [T.Text]
toInstallations = concatMap toOs . installations

toOs :: Os -> Action -> [T.Text]
toOs = map toCommand . command

-- TODO: Needs operating system and sudo awareness.
toCommand :: Command -> Action -> T.Text
toCommand c a = case a of Install -> mconcat [program c, argument c]
