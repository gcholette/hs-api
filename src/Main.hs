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

          :<|> "book" :> Capture "id" Int 
                       :> ReqBody '[JSON] Book 
                       :> Put '[JSON] Book

type UserAPI = "user" :> Get '[JSON] [Book]

server :: Server BookAPI
server = liftIO   Book.index
    :<|> liftIO . (\id -> Book.show id)
    :<|> liftIO . (\id -> Book.insert id)

    :<|> (\id body -> do
      result <- liftIO (Book.update id body)
      if result == 1
        then return body
        else throwError book404)

      where
        book404 = err404 { errBody = "Book not found."}


bookAPI :: Proxy BookAPI
bookAPI = Proxy


app :: Application
app = serve bookAPI server


main :: IO ()
main = do
  putStrLn "Starting hsb server on port 3005..."
  run 3005 app
