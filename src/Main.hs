{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import GHC.Generics
import Web.Scotty (ScottyM, ActionM, scotty, json, get, text, param)
import Data.Monoid ((<>)) -- Concatenates stff that isnt strigs
import Data.Aeson (FromJSON, ToJSON)

-- types

data Book = Book 
  { id :: String
  , title :: String  
  , author :: String
  , link :: String
  , progression :: Int 
  } deriving (Show, Generic)


instance ToJSON Book
instance FromJSON Book


-- Funcs

main :: IO ()
main = do
  putStrLn "Starting hsb server..."
  scotty 3005 routes


routes :: ScottyM ()
routes = do
  get "/books" ( json books )

  get "/book/:id" $ do 
    id <- param "id"
    json (filter (matchId id) books)


baseText :: ActionM ()
baseText = do
  text "base api."

matchId :: String -> Book -> Bool
matchId id book = (Main.id book) == id

books :: [ Book ]
books = [ book1, book2 ]

book1 :: Book
book1 = Book 
  { Main.id = "1"
  , title = "ima book"
  , author = "réjean"
  , link = "google.com"
  , progression = 15 
  }

book2 :: Book
book2 = Book 
  { Main.id = "2"
  , title = "book two I guess"
  , author = "james scott"
  , link = "google.com"
  , progression = 35 
  }