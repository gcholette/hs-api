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
  migrate createBooksTable

executeMigrations :: IO ()
executeMigrations = do
  setupDb

initDb :: IO ()
initDb = do
    conn <- connectPostgreSQL (BS8.pack postgresUrl)
    withTransaction conn $ void $ runMigration $
        MigrationContext MigrationInitialization True conn

migrate :: BS8.ByteString -> IO ()
migrate script = do
  let name = "migration script"
  conn <- connectPostgreSQL (BS8.pack postgresUrl)
  withTransaction conn $ void $ runMigration $
      MigrationContext (MigrationScript name script) True conn

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
