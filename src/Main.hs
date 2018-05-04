{-# LANGUAGE OverloadedStrings
, DeriveGeneric
, ScopedTypeVariables 
, DataKinds 
, TypeOperators 
#-}

module Main where

import Control.Monad.IO.Class
import Data.Aeson (FromJSON, ToJSON)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import GHC.Generics
import GHC.Int
import Network.Wai.Middleware.Cors
import Network.Wai.Handler.Warp (run)
import Control.Exception
import Servant

import Book
import Db
import Migration (executeMigrations)

type BookAPI = "books" :> Get '[JSON] [Book]
           :<|> "book" :> ReqBody '[JSON] Book 
                       :> Post '[JSON] Book
           :<|> "book" :> Capture "id" String 
                       :> Get '[JSON] Book

server :: Server BookAPI
server = (liftIO Book.index) 
    :<|> liftIO . Book.insert
    :<|> liftIO . Book.show

bookAPI :: Proxy BookAPI
bookAPI = Proxy

app :: Application
app = serve bookAPI server

main :: IO ()
main = do
  putStrLn "initiating migrations"
  handle (\(e :: IOException) -> putStrLn "Migrations failed" >> print e)
    executeMigrations

  putStrLn "Starting hsb server on port 3005..."
  run 3005 app
