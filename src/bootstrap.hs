-- | Copyright (c) Gary Allan Howard aka Traap.
-- License BSD-3-Clause
--
-- This program is used to bootstrap a development environment.  Bootstrapping
-- consists of two parts, namely: 1) setup symbolic links to files or
-- directories that are under version control, and 2) clone GitHub.com
-- repositories that are needed to personalize bash and vim.
{-# LANGUAGE RecordWildCards #-}

module Main (main) where
import Options.Applicative
import Traap.Options
import Traap.Workflow

-- -----------------------------------------------------------------------------
main :: IO ()
main = do 
  OPTS{..} <- execParser options 
  orchestrate fpath dryrun
