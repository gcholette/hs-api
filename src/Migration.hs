{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Migration where

import Control.Monad
import Data.Monoid ((<>)) -- Concatenates stff that isnt strigs
import Database.PostgreSQL.Simple.Migration
import Database.PostgreSQL.Simple
import qualified Data.ByteString.Char8 as BS8 

postgresUrl :: String
postgresUrl = "host=localhost dbname=hsb-db user=showlet password=abc123"

setupDb :: IO ()
setupDb = do
  initDb
 -- migrate dropTables
  migrate createBooksTable

executeMigrations :: IO ()
executeMigrations = do
  initDb

initDb :: IO ()
initDb = do
    con <- connectPostgreSQL (BS8.pack postgresUrl)
    withTransaction con $ void $ runMigration $
        MigrationContext MigrationInitialization True con

migrate :: BS8.ByteString -> IO ()
migrate script = do
  let name = "migration script"
  con <- connectPostgreSQL (BS8.pack postgresUrl)
  withTransaction con $ void $ runMigration $
      MigrationContext (MigrationScript name script) True con

dropTables :: BS8.ByteString
dropTables = "DROP TABLE books;"

createBooksTable :: BS8.ByteString
createBooksTable 
  = "CREATE TABLE books ("
  <> "id varchar PRIMARY KEY," 
  <> "title varchar,"
  <> "author varchar,"
  <> "link varchar,"
  <> "progression integer NOT NULL);"