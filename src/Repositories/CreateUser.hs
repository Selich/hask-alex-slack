{-# LANGUAGE OverloadedStrings #-}

module Repositories.CreateUser where

import Control.Lens
import Data.Text ( Text )
import Data.Text as T
import Data.Aeson
import Data.UUID
import Repositories.RequestHandler
import Network.AWS.DynamoDB.PutItem
import Network.AWS.DynamoDB.Types
import Repositories.TransformUser
import qualified Network.AWS.Env as AWSEnv
import qualified Data.List.Split as Split
import qualified Data.HashMap.Strict as HashMap
import Domain.CreateUserInput
import Data.Text.Encoding
import qualified Data.Aeson as Aeson
import Domain.User
import Util.URLEncodedParser


-- getKey userData = Prelude.fst $ ( parseQuery $ encodeUtf8 userData )!! 0
-- getVal userData = Prelude.snd $ ( parseQuery $ encodeUtf8 userData )!! 0

-- 
-- createUserParse :: AWSEnv.Env -> Maybe CreateUserInput -> IO CreateUserRes
-- createUserParse env input = 
--   case input of
--     Just input -> do
--       let userData = (input ^? body)
--       case userData of
--         Just userData -> do
--           -- let key = getKey userData
--           -- let val = getVal userData
--           -- print val
--           return $ CreateUserRes True
--         _ -> error "body"

--     nothing -> error "body"


createUsr username bday chan env= do
  res <- sendReq env q
  print $ res ^. pirsResponseStatus
  where q = putItem "lambert-dev" & piItem .~ item
        userID = generateID username
        item = HashMap.fromList
          [ ("id"       , attributeValue & avS .~ Just (generateID chan username))
          , ("sk"       , attributeValue & avS .~ Just (generateSK $ bday))
          ]

