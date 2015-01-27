{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, LambdaCase #-}
-- |This module provides a type for interacting with a
-- filesystem-backed 'MigrationStore'.
module Database.Schema.Migrations.Filesystem
    ( FilesystemStore(..)
    , migrationFromFile
    , migrationFromPath
    )
where

import System.Directory ( getDirectoryContents, doesFileExist )
import System.FilePath ( (</>), takeExtension, dropExtension
                       , takeFileName, takeBaseName )
import Control.Monad.Trans ( MonadIO, liftIO )

import Data.Time.Clock ( UTCTime )
import Data.Time () -- for UTCTime Show instance

import Control.Applicative ( (<$>), (<*>) )
import Control.Monad ( filterM, mzero )
import Data.Yaml

import Database.Schema.Migrations.Migration
    ( Migration(..)
    )
import Database.Schema.Migrations.Filesystem.Serialize
import Database.Schema.Migrations.Store

data FilesystemStore = FSStore { storePath :: FilePath }

filenameExtension :: String
filenameExtension = ".txt"

instance (MonadIO m) => MigrationStore FilesystemStore m where
    fullMigrationName s name =
        return $ storePath s </> name ++ filenameExtension

    loadMigration s theId = do
      result <- liftIO $ migrationFromFile s theId
      return $ case result of
                 Left _ -> Nothing
                 Right m -> Just m

    getMigrations s = do
      contents <- liftIO $ getDirectoryContents $ storePath s
      let migrationFilenames = [ f | f <- contents, isMigrationFilename f ]
          fullPaths = [ (f, storePath s </> f) | f <- migrationFilenames ]
      existing <- liftIO $ filterM (\(_, full) -> doesFileExist full) fullPaths
      return [ dropExtension short | (short, _) <- existing ]

    saveMigration s m = do
      filename <- fullMigrationName s $ mId m
      liftIO $ writeFile filename $ serializeMigration m

isMigrationFilename :: FilePath -> Bool
isMigrationFilename path = takeExtension path == filenameExtension

-- |Given a store and migration name, read and parse the associated
-- migration and return the migration if successful.  Otherwise return
-- a parsing error message.
migrationFromFile :: FilesystemStore -> String -> IO (Either String Migration)
migrationFromFile store name =
    fullMigrationName store name >>= migrationFromPath

data JsonMigration = JMigration
  { jmTimestamp :: UTCTime
  , jmDesc :: Maybe String
  , jmApply :: String
  , jmRevert :: Maybe String
  , jmDeps :: [String]
  } deriving (Show)

toMigration :: String -> JsonMigration -> Migration
toMigration mid jm = Migration
  {  mTimestamp = jmTimestamp jm
  ,  mId = mid
  ,  mDesc = jmDesc jm
  ,  mApply = jmApply jm
  ,  mRevert = jmRevert jm
  ,  mDeps = jmDeps jm
  }

instance FromJSON JsonMigration where
  parseJSON (Object v) = JMigration
    <$> (v .: "Created" >>= utctimeParse)
    <*> v .:? "Description"
    <*> v .: "Apply"
    <*> v .:? "Revert"
    <*> depends `fmap` (v .:? "Depends")
    where
      utctimeParse s = case reads s of 
        [(t, _)]  -> return t
        _         -> fail "could not parse ISO date"
      depends = \case
        Just s  -> words s
        Nothing -> []
  parseJSON _ = mzero

-- |Given a filesystem path, read and parse the file as a migration
-- return the 'Migration' if successful.  Otherwise return a parsing
-- error message.
migrationFromPath :: FilePath -> IO (Either String Migration)
migrationFromPath path = do
  let name = takeBaseName $ takeFileName path
  decodeFileEither path >>= \case
    Left e  -> return . Left $ show e
    Right j -> return . Right $ toMigration name j
