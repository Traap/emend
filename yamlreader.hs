-- {{{ See http://dev.stephendiehl.com/hask/#yaml
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Traap.DataTypes
import Data.Yaml
import qualified Data.ByteString as BL 
import qualified Data.Text as T


-- {{{ Main
main :: IO ()
main = do
  contents <- BL.readFile "bootstrap.yaml"

  putStrLn "\nSymoblic Links"
  mapM_ printSymLink $ decodeEither contents

  putStrLn "\nRepositories"
  mapM_ printRepo $ decodeEither contents

  putStrLn "\nInstallations"
  mapM_ printInstallations $ decodeEither contents

printSymLink :: Symlinks -> IO () 
printSymLink s = mapM_ (print' . T.unpack) $ toSymlinks s CREATE

printRepo :: Repos -> IO ()
printRepo r = mapM_ (print' . T.unpack) $ toRepos r CLONE

printInstallations :: Installations -> IO ()
printInstallations i = mapM_ (print' . T.unpack) $ toInstallations i INSTALL 

print' :: String -> IO ()
print' s = do
  putStrLn s
  return ()
---------------------------------------------------------------------------- }}}
