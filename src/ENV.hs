{-# LANGUAGE CPP, OverloadedStrings #-}

module ENV where

import System.Directory               ( canonicalizePath )
import System.FilePath.Posix          
import Configuration.Dotenv
import Data.Monoid 
import Network.AWS.Types
import Network.AWS
import Network.AWS.DynamoDB           ( dynamoDB )
import Control.Lens
import System.Environment (lookupEnv)

loadSecrets = do
  envPath <- canonicalizePath $ takeDirectory __FILE__ </> "../.env"
  loadFile $ Config [envPath] [] False


getEnvironment :: IO Env
getEnvironment = newEnv Discover
