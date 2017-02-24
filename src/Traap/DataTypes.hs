-- | Copyright (c) Gary Allan Howard aka Traap.
-- License BSD-3-Clause

module Traap.DataTypes
  (SymLink(..)
  ,Url(..)
  ,Repo(..)
  ,Command(..)
  ) where

-- -----------------------------------------------------------------------------
-- | The SymLink type defines the target of a symlink operation and identifies
-- the target type as a file or as a directory.
data SymLink = SYMLINK
  {sym  :: String -- the target for the Symlink.
  ,src  :: String -- the source of the Symlink.
  ,flag :: Bool -- true when a file; false when a directory.
  } deriving (Show)

-- -----------------------------------------------------------------------------
-- | The Url type defines a Url and whether or not the url is cloned to . (here)
-- or a new directory.
data Url = URL
  {loc  :: String -- the url string.
  ,here :: Bool   -- use . (here) versus url name to clone into.
  } deriving (Show)

-- -----------------------------------------------------------------------------
-- | The Repo type defines target directory for a git-clone operation and the
-- URL cloned from GitHub.com.
data Repo = REPO
  {tdir :: String   -- the target directory for the clone operation.
  ,url  :: [Url] -- the repositories to clone.
  } deriving (Show)

-- -----------------------------------------------------------------------------
-- | The Command type defines a program to run and the argument is is passed.
data Command = COMMAND
  {prg :: String -- A program to run.
  ,arg :: [String] -- Arguments to pass to program.
  } deriving (Show)
