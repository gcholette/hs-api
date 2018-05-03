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
import Web.Scotty (ScottyM, ActionM, scotty, middleware, text, json, get, post, options, jsonData, param, liftAndCatchIO)
import Migration (executeMigrations)
import Network.Wai.Middleware.Cors
import Control.Exception


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
connection = connectPostgreSQL "postgresql://postgres:abc123@localhost/hsb_db"


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

  get "/books" $ do
    dbBooks <- liftAndCatchIO getBooks
    liftAndCatchIO $ putStrLn "GET /books"
    json dbBooks

  get "/book/:id" $ do 
    id' <- param "id"
    dbBooks <- liftAndCatchIO getBooks
    json $ filter (matchId id') dbBooks

  post "/book" $ do
    book <- jsonData :: ActionM Book
    res <- liftAndCatchIO $ insertBook book
    json $ show res


insertBook :: Book -> IO Int64 
insertBook book = do
  conn <- connection
  execute conn "INSERT INTO books VALUES (?, ?, ?, ?, ?)" book


getBooks :: IO [Book]
getBooks = do
  conn <- connection
  query_ conn "SELECT * FROM books;"


matchId :: String -> Book -> Bool
matchId id' book = Main.id book == id'


{-
  --mapM_ print =<< ( query_ conn "select 2 + 2" :: IO [Only Int] )
  -- mapM_ :: Monad m => (a -> m b) -> [a] -> m ()

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
-}
