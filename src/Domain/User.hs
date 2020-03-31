{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DuplicateRecordFields, OverloadedStrings, TemplateHaskell #-}

module Domain.User where

import Control.Lens
import Data.Text ( Text(..) )
import Data.Aeson
import Data.Aeson.TH ( deriveJSON )
import GHC.Generics
import qualified Data.Text as T


generateID chan username = "USR#"  <> "#" <>  chan <> "#" <> username
generateSK bday          = "BRTH#" <> bday

type PK = Text
type SK = Text
type FirstName = Text
type LastName = Text

data UserOutput = UserOutput {
  _id :: Text,
  _sk :: Text
} deriving(Show, Generic)

data User = User {
  _id :: PK,
  _sk :: SK,
  _firstName :: FirstName,
  _lastName :: LastName
} deriving(Show, Generic)


deriveJSON
  defaultOptions {fieldLabelModifier = drop 1}
  ''User

makeLenses ''User


-- TODO: make this better
parseUserInput :: Maybe Text -> Maybe Text -> User
parseUserInput Nothing _       = error "first name cannot be empty"
parseUserInput _       Nothing = error "last name cannot be empty"
parseUserInput (Just firstName) (Just lastName) =
  User "1" "user" firstName lastName


