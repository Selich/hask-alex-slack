{-# LANGUAGE CPP, OverloadedStrings #-}

module ENV where

import Network.AWS
import Data.Text as Text
import Configuration.Dotenv
import System.FilePath.Posix          
import System.Environment (lookupEnv)
import System.Directory ( canonicalizePath )
import qualified Network.Linklater as Linklater


readDirFile :: FilePath -> IO Text
readDirFile fileName = Text.filter (/= '\n') . pack <$> readFile fileName

loadSecrets :: IO [(String, String)]
loadSecrets = do
  envPath <- canonicalizePath $ takeDirectory __FILE__ </> "../.env"
  loadFile $ Config [envPath] [] False

getEnvironment :: IO Env
getEnvironment = newEnv Discover

configIO :: IO Linklater.Config
configIO = Linklater.Config <$> readDirFile "hook"
