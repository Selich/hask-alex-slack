{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}


module Lib where

import GHC.Generics
-----------------------------------------------------------------------------------
import AWSLambda
import AWSLambda.Events.APIGateway
import Control.Lens
import Control.Exception
import Control.Monad.Except
import Control.Monad.Trans.AWS ( runAWST )
-----------------------------------------------------------------------------------
import Network.Linklater
import qualified Network.Linklater as Linklater
import qualified Network.AWS.Env as AWSEnv
-----------------------------------------------------------------------------------
import Prelude
import Data.Text  ( Text )
import Data.ByteString ( ByteString )
import qualified Data.Text as T
import qualified Data.Aeson as Aeson
import Data.Aeson.Embedded
-----------------------------------------------------------------------------------
import Repositories.TransformUser
import Repositories.CreateUser
import Domain.CreateUserInput
import Domain.User as User
import ENV
import Data.Aeson.Types
import qualified Data.HashMap.Strict as HashMap
-----------------------------------------------------------------------------------
import qualified Data.ByteString       as B
import Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy as Lazy
import Handlers.HandleNew
import Handlers.HandleEdit
import Handlers.HandleList
import Data.List
import Util.URLEncodedParser
-----------------------------------------------------------------------------------

configIO :: IO Linklater.Config
configIO = Linklater.Config <$> readDirFile "hook"

readDirFile :: FilePath -> IO T.Text
readDirFile fileName = T.filter (/= '\n') . T.pack <$> Prelude.readFile fileName

getWord :: [String] -> Int -> Text
getWord w num = T.pack (w !! num)

getWords :: Text -> [String]
getWords text = words( T.unpack text)

search :: Eq a => a -> [(a,b)] -> Maybe b
search a = fmap snd . find ((== a) . fst)
-----------------------------------------------------------------------------------

parseBody :: Aeson.Value -> Maybe Value
parseBody (Object obj) = case HashMap.lookup "body" obj of
    Just x  -> return x
    Nothing -> fail "no field 'body'"


liftMaybe item = case item of 
    Just x  -> 
      case x of 
        Just x  -> return  x
    Nothing -> fail "no field"

-- parseCommand :: Query -> Maybe (Maybe ByteString)
-- parseCommand t1 = do 
--   re <- case search "command" t1 of
--     Just x  -> return x
--     Nothing -> fail "no field 'command'"
--   return re

-- TODO: Get token, command
-- handler :: APIGatewayProxyRequest (Embedded CreateUserInput) -> IO (APIGatewayProxyResponse (Embedded CreateUserRes))
handler :: Aeson.Value -> IO String
handler evt = do
  env <- getEnvironment
  let body = parseBody evt
  req <- case body of
    Just x  -> return x
    Nothing -> fail "no field 'body'"
  let query =  parseQuery $ Aeson.encode req
  -- command <- liftMaybe $ search "command" query
  bs         <- liftMaybe $ search "text" query
  bsChanID   <- liftMaybe $ search "channel_id" query
  bsChan     <- liftMaybe $ search "channel_name" query
  let txt    =  Lazy.toStrict (decodeUtf8 bs)
  let chanID =  Lazy.toStrict (decodeUtf8 bsChanID)
  let chan   =  Lazy.toStrict (decodeUtf8 bsChan)
  bot txt (Linklater.Channel chanID chan)(Linklater.Config token)
  return "test"
  -- return $ responseOK & responseBodyEmbedded ?~ res
  where token = "xoxb-47546637283-940014179680-AsQ3eOGbIBGHKtHBVzb0FA45"

bot args chan token = case head w of 
         "new"  -> handleNew (getWord w 1) (getWord w 2) chan token
        --  "edit" -> handleEdit command config (getWord w 1) (getWord w 2)
        --  ""     -> handleList command config
         _      -> return "empty"
      where w = getWords $ args


-- bot ::  -> IO T.Text
-- bot command@(Command _ _ chan _) = case command of
--     (Command "bday" user channel  Nothing   ) -> return "err"
--     (Command "bday" user channel (Just text)) ->
--       case head w of 
--         --  "new"  -> handleNew  command config (getWord w 1) (getWord w 2) chan
--         --  "edit" -> handleEdit command config (getWord w 1) (getWord w 2)
--         --  ""     -> handleList command config
--          _      -> return "empty"
--       where w = getWords text

