{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables #-}

module Migration where

import Control.Monad
import Data.Monoid ((<>)) -- Concatenates stff that isnt strigs
import Database.PostgreSQL.Simple.Migration
import Database.PostgreSQL.Simple
import qualified Data.ByteString.Char8 as BS8 
import System.Directory
import GHC.Generics


data PgScript = PgScript
  { name :: ScriptName
  , content :: BS8.ByteString
  } deriving (Show, Generic)


connection :: IO (Connection)
connection = connectPostgreSQL "postgresql://postgres:abc123@localhost/hsb_db"

path :: FilePath
path = "migrationss/"

absPath :: IO FilePath
absPath = makeAbsolute path

getFileNames :: IO [FilePath]
getFileNames = do
    absPath >>= listDirectory


getFilePaths :: IO [FilePath]
getFilePaths = do
  fileNames <- getFileNames
  mapM (\file -> makeAbsolute (path ++ file)) fileNames


readMigrationScript :: String -> IO BS8.ByteString
readMigrationScript path = BS8.readFile path


executeMigrations :: IO ()
executeMigrations = do
  initDb
  fileNames  <- getFileNames
  filePaths  <- getFilePaths
  migrations <- mapM (readMigrationScript) filePaths
  mapM_ (migrate) [ PgScript (show a) b | (a, b) <- zip fileNames migrations ] 


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

