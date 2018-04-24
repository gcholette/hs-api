{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Migration where

import Control.Monad
import Data.Monoid ((<>)) -- Concatenates stff that isnt strigs
import Database.PostgreSQL.Simple.Migration
import Database.PostgreSQL.Simple
import qualified Data.ByteString.Char8 as BS8 
import System.Directory


postgresUrl :: String
postgresUrl = "host=localhost dbname=hsb-db user=showlet password=abc123"

path :: FilePath
path = "migrations/"

absPath :: IO FilePath
absPath = makeAbsolute path


getMigrationFiles :: IO [FilePath]
getMigrationFiles = do
  files <- (absPath >>= listDirectory)
  mapM (\file -> makeAbsolute (path ++ file)) files


executeMigrations :: IO ()
executeMigrations = do
  files <- getMigrationFiles
  scripts <- mapM (readMigrationScript) files
  initDb
  mapM_ (migrate) scripts


readMigrationScript :: String -> IO BS8.ByteString
readMigrationScript path = BS8.readFile path


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
