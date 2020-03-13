{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

module Handlers.HandleNew  where

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
import AWSLambda.Events.APIGateway
-----------------------------------------------------------------------------------
import GHC.Prim


newMessage cmd u bday chan = 
    SimpleMessage(EmojiIcon ":hand:") cmd chan ("Awesome, I’ve recorded @" <> u <> " birthday as " <> bday <> "!")

handleNew username bday chan token = do
  env    <- newEnv Discover
--   createUsr username bday chan token
  either <- runExceptT $ say (newMessage "/bday" username bday chan) token
  case either of
    (Left  e) -> return "error"
    (Right _) -> return ""


-- handleNew command config username bday workspace = do
--   env    <- newEnv Discover
--   res    <- createUsr env username bday workspace
--   either <- runExceptT $ say (newMessage command username bday) config
--   case either of
--     (Left  e) -> return "error"
--     (Right _) -> return ""

