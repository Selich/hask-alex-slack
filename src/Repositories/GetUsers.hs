{-# LANGUAGE OverloadedStrings #-}

module Repositories.GetUsers where

import Control.Lens
import Data.Text ( Text )
import Data.Text as T
import Data.UUID
import Repositories.TransformUser
import Repositories.RequestHandler
import Network.AWS.DynamoDB
import Network.AWS.DynamoDB.GetItem
import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Env as AWSEnv
import qualified Data.List.Split as Split
import qualified Data.HashMap.Strict as HashMap
import Domain.User


getUser :: AWSEnv.Env -> Keys -> IO (Maybe User)
getUser env keys = do
  res <- sendReq env req
  return . fromDbEntity $ res ^. girsItem
  where req = getItem "lambert-dev" & giKey .~ keys


scanTable :: AWSEnv.Env -> IO [Maybe User]
scanTable env = do
  res <- sendReq env req
  return $ Prelude.map fromDbEntity $ res ^. srsItems
  where req = scan "lambert-dev"

getUsers :: AWSEnv.Env -> Text -> IO [Maybe User]
getUsers env pk= do
  res <- sendReq env req
  return $ Prelude.map fromDbEntity $ res ^. qrsItems
 where
  req =
    query "lambder-dev"
      &  qKeyConditionExpression
      ?~ keys
      &  qExpressionAttributeValues
      .~ values
  keys   = genQueryUsersKeys
  values = genQueryUsersValues pk

