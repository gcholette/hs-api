
{-# LANGUAGE
  OverloadedStrings, 
  DeriveGeneric,
  ScopedTypeVariables,
  RecordWildCards 
#-}

module Book where

import Data.Aeson as Aeson
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import GHC.Generics
import GHC.Int
import Control.Applicative

import Db (connection)


data Book = Book 
  { id :: Maybe Int
  , title :: Maybe String  
  , author :: Maybe String
  , link :: Maybe String
  , progression :: Maybe Int 
  } deriving (Show, Generic)


instance ToJSON Book {- where
  toJSON p = object 
    [ "title" .= title p
    , "author" .= author p
    , "link" .= link p
    , "progression" .= progression p
    ] -}

instance FromJSON Book where
  parseJSON = withObject "book" $ \b -> do
    id          <- optional (b .: "id")
    title       <- b .: "title"
    author      <- b .: "author"
    link        <- b .: "link"
    progression <- b .: "progression"
    return Book{..}


instance FromRow Book where
    fromRow = Book <$> field <*> field <*> field <*> field <*> field 

instance ToRow Book where
    toRow b = 
        [ toField (Book.title b)
        , toField (Book.author b)
        , toField (Book.link b)
        , toField (Book.progression b)
        ]


index :: IO [Book]
index = do
  conn <- connection
  query_ conn indexQuery


show :: Int -> IO Book
show id = do
  conn <- connection
  books <- query conn showQuery $ Only (id :: Int)
  return (books !! 0) 


insert :: Book -> IO Book 
insert book = do
  conn <- connection
  execute conn insertQuery book
  return book


indexQuery  = "SELECT * FROM books;"
showQuery   = "SELECT * FROM books where id = ?;" 
insertQuery = "INSERT INTO books (title, author, link, progression) VALUES (?, ?, ?, ?);"


update :: Book -> IO Book 
update book = do
  conn <- connection
  execute conn "UPDATE books SET author = ? where id = ?" book
  return book