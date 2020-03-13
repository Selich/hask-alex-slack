{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

module Handlers.HandleList  where

import GHC.Generics
import Data.Text  ( Text )
import Data.Aeson
import Data.HashMap.Strict as HashMap
-----------------------------------------------------------------------------------
import Control.Lens
import Control.Exception
import Control.Monad.Except
import Control.Monad.Trans.AWS ( runAWST )
-----------------------------------------------------------------------------------
import Network.Linklater
-----------------------------------------------------------------------------------
import Network.AWS.DynamoDB.PutItem
import qualified Network.AWS.Env as AWSEnv
import Network.AWS.DynamoDB
import Network.AWS.Types
import Network.AWS
-----------------------------------------------------------------------------------
import qualified AWSLambda as AWS
-----------------------------------------------------------------------------------
import Prelude
import Data.Text
-----------------------------------------------------------------------------------
import qualified Domain.User as U
import Repositories.RequestHandler
import Repositories.TransformUser
import Repositories.GetUsers
import ENV
import Data.Aeson.Embedded
import AWSLambda.Events.APIGateway
-----------------------------------------------------------------------------------
import Domain.User
import Data.Aeson                     ( encode )
import GHC.Prim

newtype GetUsersUseCaseRes = GetUsersUseCaseRes {users :: [Maybe U.User]} deriving(ToJSON, Generic, Show)

listMessage (Command cmd usr channel text) res = 
    SimpleMessage(EmojiIcon ":hand:") cmd channel "test"

handleList command config = do
  env <- newEnv Discover
  res <- getUsers env "USR"
  eitherResult <- runExceptT $ say (listMessage command "test") config
  case eitherResult of
    (Left e)  -> return "error"
    (Right _) -> return ""

