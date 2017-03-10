-- | Copyright (c) Gary Allan Howard aka Traap.
-- License BSD-3-Clause

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Traap.Workflow (orchestrate) where

import           Control.Monad
import qualified Data.ByteString as BL
import qualified Data.Text as T
import           Data.Yaml
import           GHC.Generics
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.Process
import           Traap.DataTypes

-- -----------------------------------------------------------------------------
-- | Orchestrate cloning GitHub.com repositories and creating symbolic links.
-- Step 1: Remove everything we are about to create.
-- Step 2: Clone repositories from github.
-- Step 3: Create symbolic links.
orchestrate :: IO ()
orchestrate = do
  contents <- BL.readFile "bootstrap.yaml"
  mapM_ deleteSymLink $ decodeEither contents
  -- mapM_ withDirCloneRepo $ repos github
  mapM_ makeSymLink $ decodeEither contents

-- -----------------------------------------------------------------------------
-- | Recursively delete objects referenced by SymLink.
deleteSymLink :: SymLink -> IO ExitCode
deleteSymLink = map system $ toSymlink Delete

-- -----------------------------------------------------------------------------
-- | Create symbolic links for objects referenced by SymLink.
-- Concatenate source file name (ex: ~/git/dotfiles/bashrc).
makeSymLink :: [T.Text] -> IO ExitCode
makeSymLink = map system $ toSymlink Create

-- -----------------------------------------------------------------------------
-- | Setup directory to clone repository into.
withDirCloneRepo :: Repo -> IO ()
withDirCloneRepo r = do
  setDotFileDirectory
  setCloneDirectory (tdir r)
--  mapM_ cloneRepo (url r)
  setDotFileDirectory

-- -----------------------------------------------------------------------------
-- | Setup directory.
setCloneDirectory :: FilePath -> IO ()
setCloneDirectory fpath = do
  safelyRemoveDirectory fpath
  createDirectoryIfMissing True fpath
  setCurrentDirectory fpath

-- -----------------------------------------------------------------------------
-- | Set ~/dotfiles/vim directory
setDotFileDirectory :: IO ()
setDotFileDirectory = do
  h <- getHomeDirectory
  let f = h ++ "/git/dotfiles"
  setCurrentDirectory f

-- -----------------------------------------------------------------------------
-- | Safely remove the directory and all sub-folders.
safelyRemoveDirectory :: FilePath -> IO ()
safelyRemoveDirectory fpath = do
  b <- doesDirectoryExist fpath
  Control.Monad.when b $ removeDirectoryRecursive fpath

-- -----------------------------------------------------------------------------
-- | Clone repos I am interested in using.
-- The url cloned has two formats
-- 1) clone git@github.com:Traap/dotfiles
--    This invocation creates a new directory named dotfiles and clones into it.
--
-- 2) clone git@github.com:Traap/dotfiles .
--    This invocation clones into the current directory.
--
-- cloneRepo :: Url -> IO ExitCode
-- cloneRepo u = do
--   let s = if here u then loc u ++ " ." else loc u
--   system s
