{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Bot where

import Control.Monad.Trans.AWS ( runAWST )
import Prelude hiding (filter, lookup)
import Data.Text ( Text, filter, pack, unpack , splitOn)
import Data.Aeson
import qualified Data.ByteString.Lazy as BLazy
import Data.ByteString.Lazy.UTF8
import Data.HashMap.Strict hiding (filter, map)
import qualified Data.Text.Lazy as Lazy
import Data.Text.Lazy.Encoding
import Util.URLEncodedParser
import qualified Crypto.Hash.SHA256 as Hash

import Network.AWS
import Control.Monad.IO.Class
import Network.AWS.DynamoDB
import qualified Network.Linklater as Linklater
import Domain.CreateUserInput
import Handlers
import ENV

getWord :: [String] -> Int -> Text
getWord w num = pack (w !! num)

getWords :: Text -> [String]
getWords text = words( unpack text)

parseBody :: Value -> Maybe Value
parseBody (Object obj) = case lookup "body" obj of
    Just x  -> return x
    Nothing -> fail "no field 'body'"

-- TODO: Get token from DynamoDB
handler :: Value -> IO ()
handler event = do
  -- token
  token <- readDirFile ".env"

  body   <- case parseBody event of
    Just x  -> return x
    Nothing -> fail "No field 'body'"

  let query  =  parseQuery $ encode body
  bs         <- liftMaybe $ search "text" query
  bsChanID   <- liftMaybe $ search "channel_id" query
  bsChan     <- liftMaybe $ search "channel_name" query
  let args    = splitOn "+" $ Lazy.toStrict (decodeUtf8 bs)
  let chanID =  Lazy.toStrict (decodeUtf8 bsChanID)
  let chan   =  Lazy.toStrict (decodeUtf8 bsChan)
  bot chanID chan (Linklater.Config token) args

bot :: Text -> Text -> Linklater.Config -> [Text] -> IO()
bot chanID chan config args = case head args of
         "new"   -> handleNew   (args !! 1) chan chanID config
         "edit"  -> handleEdit  (args !! 1) chan chanID config
         ""      -> handleList  (args !! 1) chan chanID config
         _       -> putStrLn "Err"