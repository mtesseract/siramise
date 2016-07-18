{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings   #-}

module Lib
    ( startApp
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

data Function = Addition | Subtraction

instance FromHttpApiData Function where
  parseUrlPiece "add" = Right Addition
  parseUrlPiece "subtract" = Right Subtraction
  parseUrlPiece _ = Left "unknown function"
  parseQueryParam _ = Left "not supported"

type API = "users" :> Get '[JSON] [User]
      :<|> "calculator" :> Capture "function" Function
                        :> Capture "a" Integer
                        :> Capture "b" Integer
                        :> Get '[JSON] Integer
      :<|> "echo"       :> ReqBody '[PlainText] String :> Post '[PlainText] String

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = users
    :<|> calculator
    :<|> echo

echo :: String -> Handler String
echo = return

calculator :: Function -> Integer -> Integer -> Handler Integer
calculator Addition a b = return $ a + b
calculator Subtraction a b = return $ a - b

users :: Handler [User]
users = return
  [ User 1 "Isaac" "Newton"
  , User 2 "Albert" "Einstein"
  ]
