{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables #-}

module Migration where

import Control.Monad
import Data.Monoid ((<>)) -- Concatenates stff that isnt strigs
import Database.PostgreSQL.Simple.Migration
import Database.PostgreSQL.Simple
import qualified Data.ByteString.Char8 as BS8 
import System.Directory
import GHC.Generics
import Data.List

import Db

data PgScript = PgScript
  { name :: ScriptName
  , content :: BS8.ByteString
  } deriving (Show, Generic)


migrationsPath :: FilePath
migrationsPath = "db/migrations/"


setup :: IO ()
setup = do
  initDb
  createSchema
  migrations
  seedDatabase


createSchema :: IO () 
createSchema = do
  schema <- readMigrationScript "db/schema.sql"
  migrate (PgScript "schema" schema)


migrations :: IO ()
migrations = do
  fileNames  <- getFileNames
  filePaths  <- getFilePaths
  migrations <- mapM (readMigrationScript) filePaths
  mapM_ (migrate) [ PgScript (show a) b | (a, b) <- zip (fileNames) migrations ] 
    where
      absPath = makeAbsolute migrationsPath

      getFileNames = do
          absPath >>= listDirectory

      getFilePaths = do
        fileNames <- getFileNames
        mapM (\file -> makeAbsolute (migrationsPath ++ file)) fileNames


seedDatabase :: IO () 
seedDatabase = do
  seeds <- readMigrationScript "db/seeds.sql"
  migrate (PgScript "seeds" seeds)


readMigrationScript :: FilePath -> IO BS8.ByteString
readMigrationScript path = BS8.readFile path


initDb :: IO ()
initDb = do
    conn <- connection
    withTransaction conn $ void $ runMigration $
        MigrationContext MigrationInitialization True conn


migrate :: PgScript -> IO ()
migrate (PgScript name content) = do
  conn <- connection
  withTransaction conn $ void $ runMigration $
      MigrationContext (MigrationScript name content) True conn

