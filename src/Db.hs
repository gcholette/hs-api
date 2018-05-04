{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables #-}

module Db where

import Database.PostgreSQL.Simple

connection :: IO (Connection)
connection = connectPostgreSQL "postgresql://postgres:abc123@localhost/hsb_db"