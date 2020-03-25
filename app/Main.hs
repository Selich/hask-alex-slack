module Main where

import Bot
import AWSLambda
import AWSLambda.Events.APIGateway

main :: IO()
main = lambdaMain handler
