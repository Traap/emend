-- | Copyright (c) Gary Allan Howard aka Traap.
-- License BSD-3-Clause

module Traap.Options (
  -- Command line arguments
  Opts(..)
  ,options
  ) where

import           Data.Semigroup ((<>))
import           Options.Applicative

-- -----------------------------------------------------------------------------
data Opts = OPTS
  { fpath :: String
  , dryrun :: Bool
  }

-- -----------------------------------------------------------------------------
options :: ParserInfo Opts
options = info (oparse <**> helper)
  (fullDesc
  <> progDesc "Use bootstrap to configure your development environment."
  <> header "bootstrap - Copyright 2017"
  )

-- -----------------------------------------------------------------------------
oparse :: Parser Opts
oparse = OPTS
  <$> strOption
    ( long "file"
    <> short 'f'
    <> metavar "file"
    <> value "bootstrap.yaml"
    <> showDefault
    <> help "Input configuration file."
    )
  <*> switch
    ( long "dryrun"
    <> short 'd'
    <> showDefault
    <> help "Print configuration without running commands."
    )
