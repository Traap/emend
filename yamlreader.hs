-- {{{ See http://dev.stephendiehl.com/hask/#yaml
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
---------------------------------------------------------------------------- }}}
-- {{{ Required imports
import           Data.Yaml
import qualified Data.Text as T
import qualified Data.ByteString as BL
import           GHC.Generics
---------------------------------------------------------------------------- }}}
-- {{{ Symbolic Links
data Symlinks = SYMLINKS
  { symlinks :: [Symlink]
  } deriving (Show, Generic, FromJSON)

data Symlink = SYMLINK
  { file :: T.Text
  , link :: T.Text
  , directory :: Bool
  } deriving (Show, Generic, FromJSON)
---------------------------------------------------------------------------- }}}
-- {{{ Repositories
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
---------------------------------------------------------------------------- }}}
-- {{{ Installations
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
---------------------------------------------------------------------------- }}}
-- {{{ toSymlinks
toSymlinks :: Symlinks -> [T.Text]
toSymlinks = map toSymlink . symlinks
---------------------------------------------------------------------------- }}}
-- {{{ toSymlink
toSymlink :: Symlink -> T.Text
toSymlink s = mconcat ["ln -s", file s, " ", link s] 
---------------------------------------------------------------------------- }}}
-- {{{ toRepos
toRepos :: Repos -> [T.Text]
toRepos = concatMap toRepo . repos 
---------------------------------------------------------------------------- }}}
-- {{{ toRepo
toRepo :: Repo -> [T.Text]
toRepo REPO {..} = map (toPath url) paths
---------------------------------------------------------------------------- }}}
-- {{{ toPath
toPath :: T.Text -> Path -> T.Text
toPath u p = mconcat ["git clone ", u, "/", source p, target p] 
---------------------------------------------------------------------------- }}}
-- {{{ toInstallations
toInstallations :: Installations -> [T.Text]
toInstallations = concatMap toOs . installations 
---------------------------------------------------------------------------- }}}
-- {{{ toOs
toOs :: Os -> [T.Text]
toOs = map toCommand . command 
---------------------------------------------------------------------------- }}}
-- {{{ toCommand
toCommand :: Command -> T.Text
toCommand c = mconcat [program c, argument c] 
---------------------------------------------------------------------------- }}}
-- {{{ Main
main :: IO ()
main = do
  contents <- BL.readFile "bootstrap.yaml"

  putStrLn "\nSymoblic Links"
  either putStrLn (print . toSymlinks) $ decodeEither contents

  putStrLn "\nRepositories"
  either putStrLn (print . toRepos) $ decodeEither contents

  putStrLn "\nInstallations"
  either putStrLn (print . toInstallations) $ decodeEither contents
---------------------------------------------------------------------------- }}}
