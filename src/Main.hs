{-# LANGUAGE 
  OverloadedStrings,
  DeriveGeneric,
  ScopedTypeVariables, 
  DataKinds, 
  TypeOperators 
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

type BookAPI = "books" :> Get '[JSON] [Book]
          :<|> "book" :> Capture "id" Int 
                      :> Get '[JSON] Book
          :<|> "book" :> ReqBody '[JSON] Book 
                      :> Post '[JSON] Book

        --  :<|> "book" :> Capture "id" String 
        --              :> ReqBody '[JSON] Book 
        --              :> Get '[JSON] Book

type UserAPI = "user" :> Get '[JSON] [Book]

server :: Server BookAPI
server = liftIO  (indexmsg -- yay crappy logging
                      >> Book.index)
    :<|> liftIO . (\id -> (showmsg id) 
                      >> (Book.show id))
    :<|> liftIO . (\id -> insertmsg 
                      >> (Book.insert id))
    -- :<|> liftIO . Book.insert
      where
        indexmsg   = putStrLn "GET /books"
        showmsg id = putStrLn $ "GET /book" ++ (Prelude.show id)
        insertmsg  = putStrLn "POST /book"


bookAPI :: Proxy BookAPI
bookAPI = Proxy


app :: Application
app = serve bookAPI server


main :: IO ()
main = do
  putStrLn "Starting hsb server on port 3005..."
  run 3005 app
