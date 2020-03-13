{-# LANGUAGE DuplicateRecordFields, OverloadedStrings #-}

module Repositories.TransformUser where

import Network.AWS.DynamoDB
import Control.Lens
import qualified Domain.User as User
import qualified Data.HashMap.Strict as HM
import Data.Text ( Text(..) )
import Data.Maybe ( fromJust , isNothing)

type Keys = HM.HashMap Text AttributeValue

genGetUserKeys :: Text -> Keys
genGetUserKeys id = HM.fromList
  [("id", attributeValue & avS ?~ id), ("sk", attributeValue & avS ?~ "user")]

genQueryUsersKeys = "id=:id AND sk:sk" :: Text
genQueryUsersValues userId = HM.fromList
  [ (":id", attributeValue & avS ?~ userId)
  , (":sk", attributeValue & avS ?~ "user")
  ]

fromDbEntity :: HM.HashMap Text AttributeValue -> Maybe User.User
fromDbEntity item = if null item
  then Nothing
  else Just User.User { _id       = fromJust $ item ^. ix "id" . avS
                      , _sk        = fromJust $ item ^. ix "sk" . avS
                      }


toDbEntity :: User.User -> HM.HashMap Text AttributeValue
toDbEntity user = HM.fromList
  [ ("id"       , attributeValue & avS .~ user ^? User.id)
  , ("sk"       , attributeValue & avS .~ user ^? User.sk)
  ]