-- | Copyright (c) Gary Allan Howard aka Traap.
-- License BSD-3-Clause

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

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
  mapM_ cloneRepo $ decodeEither contents
  mapM_ makeSymLink $ decodeEither contents
  mapM_ install $ decodeEither contents

-- -----------------------------------------------------------------------------
-- | Recursively delete objects referenced by SymLink.
deleteSymLink :: Symlinks -> IO () 
deleteSymLink s = mapM_ system $ toSymlinks s DELETE

-- -----------------------------------------------------------------------------
-- | Create symbolic links for objects referenced by SymLink.
-- Concatenate source file name (ex: ~/git/dotfiles/bashrc).
makeSymLink :: Symlinks -> IO () 
makeSymLink s = mapM_ system $ toSymlinks s CREATE

-- -----------------------------------------------------------------------------
-- | Clone repositories
cloneRepo :: Repos -> IO ()
cloneRepo r = do
  mapM_ safelyRemoveDirectory $ toRepos r DELETE
  mapM_ clone $ toRepos r CREATE

-- -----------------------------------------------------------------------------
-- | Safely remove the directory and all sub-folders.
safelyRemoveDirectory :: T.Text -> IO ()
safelyRemoveDirectory f = do
  b <- doesDirectoryExist f
  Control.Monad.when b $ mapM_ removeDirectoryRecursive f

-- -----------------------------------------------------------------------------
-- | Clone a repository.
clone :: T.Text -> IO ()
clone = mapM_ system

-- -----------------------------------------------------------------------------
-- | Install other program
install :: Os -> IO ()
install o = mapM_ system $ toOs o Install
 
