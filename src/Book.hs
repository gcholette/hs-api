
{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables #-}

module Book where

import Data.Aeson (FromJSON, ToJSON)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import GHC.Generics
import GHC.Int

import Db (connection)

data Book = Book 
  { id :: String
  , title :: String  
  , author :: String
  , link :: String
  , progression :: Int 
  } deriving (Show, Generic)

instance ToJSON Book
instance FromJSON Book

instance FromRow Book where
    fromRow = Book <$> field <*> field <*> field <*> field <*> field 

instance ToRow Book where
    toRow b = 
        [ toField (Book.id b)
        , toField (Book.title b)
        , toField (Book.author b)
        , toField (Book.link b)
        , toField (Book.progression b)
        ]


insert :: Book -> IO Int64 
insert book = do
  conn <- connection
  execute conn "INSERT INTO books VALUES (?, ?, ?, ?, ?)" book


getAll :: IO [Book]
getAll = do
  conn <- connection
  query_ conn "SELECT * FROM books;"


matchId :: String -> Book -> Bool
matchId id' book = Book.id book == id'