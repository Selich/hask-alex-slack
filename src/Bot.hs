{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Bot where

import Control.Monad.Trans.AWS ( runAWST )
import Prelude hiding (filter, lookup)
import Data.Text ( Text, filter, pack, unpack )
import Data.Aeson
import Data.HashMap.Strict hiding (filter, map)
import qualified Data.Text.Lazy as Lazy
import Data.Text.Lazy.Encoding
import Util.URLEncodedParser
import Network.AWS
import Network.AWS.DynamoDB
import qualified Network.Linklater as Linklater
import Domain.CreateUserInput
import Handlers
import ENV

sendReq env = runResourceT . runAWS env . send

getWord :: [String] -> Int -> Text
getWord w num = pack (w !! num)

getWords :: Text -> [String]
getWords text = words( unpack text)

parseBody :: Value -> Maybe Value
parseBody (Object obj) = case lookup "body" obj of
    Just x  -> return x
    Nothing -> fail "no field 'body'"

handler :: Value -> IO Text
handler event = do
  token <- readDirFile "hook"
  env   <- getEnvironment
  req   <- case parseBody event of
    Just x  -> return x
    Nothing -> fail "No field 'body'"
  let query  =  parseQuery $ encode req
  bs         <- liftMaybe $ search "text" query
  bsChanID   <- liftMaybe $ search "channel_id" query
  bsChan     <- liftMaybe $ search "channel_name" query
  let txt    =  Lazy.toStrict (decodeUtf8 bs)
  let chanID =  Lazy.toStrict (decodeUtf8 bsChanID)
  let chan   =  Lazy.toStrict (decodeUtf8 bsChan)
  print chanID
  bot txt (Linklater.Channel chanID chan)(Linklater.Config token)
  return "test"

bot args chan token = case head w of
         "new"  -> handleNew  (getWord w 1) (getWord w 2) chan token
         "edit" -> handleEdit (getWord w 1) (getWord w 2) chan token
         ""     -> handleList (getWord w 1) (getWord w 2) chan token
         _      -> return "empty"
      where w = getWords args
