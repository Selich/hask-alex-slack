module Util.TokenParser where

-- fromDbToken :: HashMap.HashMap Text AttributeValue -> Maybe Text
-- fromDbToken item = if null item
--   then Nothing
--   else Just item ^. "access_token" .avS

-- getToken :: AWSEnv.Env -> Text -> IO (Maybe Text)
-- getToken env keys = do
--   res <- sendReq env req
--   return . fromDbToken $ res ^. girsItem
--   where req = getItem "lambert-dev" & giKey keys

-- getSlackToken :: AWSEnv.Env -> Text -> IO Text
-- getSlackToken env key = do
--     let keys = getToken key
--     token <- getToken env keys
    -- return token