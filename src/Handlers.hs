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
import Control.Lens
import Control.Monad.Except
import Control.Monad.Trans.AWS ( runAWST )
import Network.Linklater
import Network.AWS.DynamoDB.PutItem
import Data.Text
import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Env as AWSEnv
import Network.AWS
import Prelude
import Domain.User
import GHC.Prim
import ENV

sendReq env = runResourceT . runAWS env . send


-- newMessage username bday chan = 
--     FormattedMessage (EmojiIcon ":hand:") "bday" chan (Format )
newMessage username bday chan = 
    SimpleMessage (EmojiIcon ":hand:") "bday" chan ("Awesome, I’ve recorded @" <> username <> " birthday as " <> bday <> "!")

editMessage username bday chan = 
    SimpleMessage (EmojiIcon ":hand:") "bday" chan ("Awesome, I’ve recorded @" <> username <> " birthday as " <> bday <> "!")

listMessage username bday chan = 
    SimpleMessage (EmojiIcon ":hand:") "bday" chan ("Awesome, I’ve recorded @" <> username <> " birthday as " <> bday <> "!")

-- Add userID as a key 
handleNew username chan chanID config = do
   env <- getEnvironment 
--    bday <- Data.Text.Text "bday"
   res <- sendReq env q
   either <- runExceptT $ (say (newMessage username "bday" (Channel chanID chan)) config)
   case either of
    Left e -> putStrLn ("an error occurred! " <> show e)
    Right _ -> putStrLn ""
   where q = putItem "lambert-dev" & piItem .~ item
         userID = generateID chan username
         item = HashMap.fromList
          [ ("id"       , attributeValue & avS .~ Just (generateID chanID username))
        --   , ("sk"       , attributeValue & avS .~ Just (generateSK $ bday))
          ]

-- handleEdit username chan chanID config = do
handleEdit username chan chanID config = undefined
--    either <- runExceptT $ (say (editMessage username bday (Channel chanID chan)) config)
--    case either of
--     Left e -> putStrLn ("an error occurred! " <> show e)
--     Right _ -> putStrLn ""

handleList username chan chanID config = undefined
-- handleList username chan chanID config = do
--    either <- runExceptT $ (say (listMessage username bday (Channel chanID chan)) config)
--    case either of
--     Left e -> putStrLn ("an error occurred! " <> show e)
--     Right _ -> putStrLn ""