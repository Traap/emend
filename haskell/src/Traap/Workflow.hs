-- | Copyright (c) Gary Allan Howard aka Traap.
-- License BSD-3-Clause

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Traap.Workflow (orchestrate) where

import qualified Data.ByteString as BL
import qualified Data.Text as T
import           Data.Yaml
import           System.Process
import           Traap.DataTypes

-- -----------------------------------------------------------------------------
-- | Orchestrate cloning GitHub.com repositories and creating symbolic links.
-- Step 1: Remove everything we are about to create.
-- Step 2: Clone repositories from github.
-- Step 3: Create symbolic links.
orchestrate :: String -> Bool ->  IO ()
orchestrate ofile oflag = do

  if oflag then putStrLn "dryrun: True"
           else putStrLn "dryrun: False"

  contents <- BL.readFile ofile 
  mapM_ deleteSymLink $ decodeEither contents
  mapM_ cloneRepo $ decodeEither contents
  mapM_ makeSymLink $ decodeEither contents
  mapM_ install $ decodeEither contents

-- -----------------------------------------------------------------------------
-- | Recursively delete objects referenced by SymLink.
deleteSymLink :: Symlinks -> IO () 
deleteSymLink s = mapM_ (system' . T.unpack) $ toSymlinks s DELETE

-- -----------------------------------------------------------------------------
-- | Create symbolic links for objects referenced by SymLink.
-- Concatenate source file name (ex: ~/git/dotfiles/bashrc).
makeSymLink :: Symlinks -> IO () 
makeSymLink s = mapM_ (system' . T.unpack) $ toSymlinks s CREATE

-- -----------------------------------------------------------------------------
-- | Clone repositories
cloneRepo :: Repos -> IO ()
cloneRepo r = do
  mapM_ (system' . T.unpack) $ toRepos r DELETE
  mapM_ clone $ toRepos r CLONE

-- -----------------------------------------------------------------------------
-- | Clone a repository.
clone :: T.Text -> IO ()
clone f = do 
  _ <- system' $ T.unpack f
  return ()

-- -----------------------------------------------------------------------------
-- | Install other program
install :: Installations -> IO ()
install i = mapM_ (system' . T.unpack) $ toInstallations i INSTALL

-- -----------------------------------------------------------------------------
-- Print a string and invoke the system command.
system' :: String -> IO ()
system' s = do
  putStrLn $ "System command: " ++ s
  _ <- system s
  return ()