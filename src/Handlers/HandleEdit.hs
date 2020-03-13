{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

module Handlers.HandleEdit  where

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
-----------------------------------------------------------------------------------
import qualified Domain.User as U
import Repositories.RequestHandler
import Repositories.TransformUser
import Repositories.CreateUser
import ENV
import Data.Aeson.Embedded
import AWSLambda.Events.APIGateway


-----------------------------------------------------------------------------------
import GHC.Prim

newtype GetUsersUseCaseRes = GetUsersUseCaseRes {users :: [Maybe U.User]} deriving(ToJSON, Generic, Show)


editMessage (Command cmd usr channel text) u bday = SimpleMessage(EmojiIcon ":hand:") cmd channel ("Ok, I’ve edited the birthday for @" <> u <>" to " <> bday)


handleEdit command config username bday = do
  env <- newEnv Discover
  -- res <- createUser env item
  eitherResult <- runExceptT $ say (editMessage command username bday) config
  case eitherResult of
    (Left e)  -> return "error"
    (Right _) -> return ""
  where 
      item = HashMap.fromList
        [ ("id"       , attributeValue & avS .~ Just "123")
        , ("sk"       , attributeValue & avS .~ Just "9876")
        , ("username" , attributeValue & avS .~ Just username)
        , ("bday"     , attributeValue & avS .~ Just bday)
        ]