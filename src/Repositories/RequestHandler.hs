module Repositories.RequestHandler where
    
import Network.AWS

sendReq env = runResourceT . runAWS env . send