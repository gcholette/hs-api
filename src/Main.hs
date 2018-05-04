{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables #-}

module Main where

import Control.Monad.IO.Class
import Data.Aeson (FromJSON, ToJSON)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import GHC.Generics
import GHC.Int
import Web.Scotty as API (ScottyM, ActionM, scotty, middleware, text, json, get, post, options, jsonData, param, liftAndCatchIO)
import Network.Wai.Middleware.Cors
import Control.Exception

import Book
import Db
import Migration (executeMigrations)


main :: IO ()
main = do
  putStrLn "initiating migrations"
  handle (\(e :: IOException) -> putStrLn "Migrations failed" >> print e)
    executeMigrations

  putStrLn "Starting hsb server"
  scotty 3005 routes


routes :: ScottyM ()
routes = do
  middleware simpleCors

  API.get "/books" $ do
    dbBooks <- liftAndCatchIO Book.getAll
    liftAndCatchIO $ putStrLn "GET /books"
    json dbBooks

  API.get "/book/:id" $ do 
    id' <- param "id"
    dbBooks <- liftAndCatchIO Book.getAll
    json $ filter (matchId id') dbBooks

  API.post "/book" $ do
    book <- jsonData :: ActionM Book
    res <- liftAndCatchIO $ Book.insert book
    json $ show res
