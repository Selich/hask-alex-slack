
module Main where
-----------------------------------------------------------------------------------
import Data.String
-----------------------------------------------------------------------------------
import Text.Show
import Network.Linklater
import ENV (loadSecrets, getEnvironment)
import Data.Bool
import GHC.Generics
-----------------------------------------------------------------------------------
import AWS.Lambda.Context (LambdaContext(..))
import AWS.Lambda.Runtime
-----------------------------------------------------------------------------------
import Lib
import Data.Aeson (FromJSON, object)
import qualified Data.Aeson as Aeson
import System.IO
import AWSLambda.Events.APIGateway
import Domain.User
import Data.Aeson.Embedded
import Data.Text                      ( Text )
import Control.Lens
import qualified Data.ByteString.Lazy          as BL
import Text.Show.Pretty
import Domain.CreateUserInput
import AWSLambda
import System.Environment (lookupEnv)
import Repositories.CreateUser
import Data.IORef
import Web.Slack

main = lambdaMain handler
-- main = apiGatewayMain handler
