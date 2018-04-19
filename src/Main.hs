{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where

import Data.Aeson (FromJSON, ToJSON)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import GHC.Generics
import Web.Scotty (ScottyM, ActionM, scotty, json, get, text, param)
import Migration (executeMigrations, initDb)


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
    toRow b = [toField (Main.id b), toField (Main.title b), toField (Main.author b), toField (Main.link b), toField (Main.progression b)]

connection :: IO (Connection)
connection = connectPostgreSQL "postgresql://postgres:abc123@localhost/hsb-db"

main :: IO ()
main = do
  putStrLn "help"
  initDb
--  putStrLn "Connecting to postgres..."
--  conn <- connection

--  putStrLn "Setting up db"
--  setupDb

--  putStrLn "INSERT book"
--  execute conn "INSERT INTO books VALUES (?, ?, ?, ?, ?)" book1

--  putStrLn "SELECT * FROM BOOKS"
--  value <- query_ conn "select * from books"
--  print (value :: [Book])

  --mapM_ print =<< ( query_ conn "select 2 + 2" :: IO [Only Int] )
  -- mapM_ :: Monad m => (a -> m b) -> [a] -> m ()

  putStrLn "Starting hsb server..."
  scotty 3005 routes


routes :: ScottyM ()
routes = do
  get "/books" ( json books )

  get "/book/:id" $ do 
    id' <- param "id"
    json (filter (matchId id') books)


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
