{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Yaml
import qualified Data.ByteString as BL 
import qualified Data.Text as T
import Options.Applicative
import Traap.DataTypes
import Traap.Options

main :: IO ()
main = do
  OPTS{..} <- execParser options 
  contents <- BL.readFile fpath 
  mapM_ printSymLink       $ decodeEither contents
  mapM_ printRepo          $ decodeEither contents
  mapM_ printInstallations $ decodeEither contents

printSymLink :: Symlinks -> IO () 
printSymLink s = do 
  putStrLn "\nSymoblic Links"
  mapM_ (print' . T.unpack) $ toSymlinks s CREATE

printRepo :: Repos -> IO ()
printRepo r = do
  putStrLn "\nRepositories"
  mapM_ (print' . T.unpack) $ toRepos r CLONE

printInstallations :: Installations -> IO ()
printInstallations i = do
  putStrLn "\nInstallations"
  mapM_ (print' . T.unpack) $ toInstallations i INSTALL 

print' :: String -> IO ()
print' s = do
  putStrLn s
  return ()
