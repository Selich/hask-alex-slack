{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DuplicateRecordFields, OverloadedStrings, TemplateHaskell #-}

module Domain.CreateUserInput where

import           Data.Aeson.Embedded
import           Control.Lens
import           Data.Aeson
import           GHC.Generics
import           Data.Text                      ( Text )
import           Data.ByteString

data CreateUserInput = CreateUserInput {
    _body :: Text
} deriving (ToJSON, FromJSON, Generic, Show)

makeLenses ''CreateUserInput

newtype CreateUserRes = CreateUserRes {success :: Bool} deriving(ToJSON, Generic, Show)
