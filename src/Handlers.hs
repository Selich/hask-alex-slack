{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

module Handlers where

import GHC.Generics
import Data.HashMap.Strict as HashMap
import Control.Exception
import Control.Monad.Except
import Control.Monad.Trans.AWS ( runAWST )
import Network.Linklater
import qualified Network.AWS.Env as AWSEnv
import Network.AWS
import Prelude
import GHC.Prim


newMessage cmd u bday chan = 
    SimpleMessage(EmojiIcon ":hand:") cmd chan ("Awesome, I’ve recorded @" <> u <> " birthday as " <> bday <> "!")

editMessage cmd u bday chan = 
    SimpleMessage(EmojiIcon ":hand:") cmd chan ("Awesome, I’ve recorded @" <> u <> " birthday as " <> bday <> "!")

listMessage cmd u bday chan = 
    SimpleMessage(EmojiIcon ":hand:") cmd chan ("Awesome, I’ve recorded @" <> u <> " birthday as " <> bday <> "!")

handleNew username bday chan token = do
  env    <- newEnv Discover
  either <- runExceptT $ say (newMessage "/bday" username bday chan) token
  case either of
    (Left  e) -> return "error"
    (Right _) -> return ""

handleEdit username bday chan token = do
  env    <- newEnv Discover
  either <- runExceptT $ say (editMessage "/bday" username bday chan) token
  case either of
    (Left  e) -> return "error"
    (Right _) -> return ""

handleList username bday chan token = do
  env    <- newEnv Discover
  either <- runExceptT $ say (listMessage "/bday" username bday chan) token
  case either of
    (Left  e) -> return "error"
    (Right _) -> return ""

-- TODO
--       item = HashMap.fromList
--         [ ("id"       , attributeValue & avS .~ Just "123")
--         , ("sk"       , attributeValue & avS .~ Just "9876")
--         , ("username" , attributeValue & avS .~ Just username)
--         , ("bday"     , attributeValue & avS .~ Just bday)
--         ]