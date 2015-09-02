{-# LANGUAGE CPP #-}

module BackendTest where

#ifndef WithoutBackendDependencies

import Test.HUnit
import Control.Monad ( forM_ )

import Database.Schema.Migrations.Backend.HDBC
import Database.Schema.Migrations.Migration ( Migration(..), newMigration )
import Database.Schema.Migrations.Backend ( Backend(..) )

import Database.HDBC ( IConnection(..), catchSql, withTransaction )

tests :: (IConnection a) => a -> IO ()
tests conn = do
  let acts = [ isBootstrappedFalseTest
             , bootstrapTest
             , isBootstrappedTrueTest
             , applyMigrationFailure
             , applyMigrationSuccess
             , revertMigrationFailure
             , revertMigrationNothing
             , revertMigrationJust
             ]
  forM_ acts $ \act -> do
               commit conn
               act conn

bootstrapTest :: (IConnection a) => a -> IO ()
bootstrapTest conn = do
  let backend = hdbcBackend conn
  bs <- getBootstrapMigration backend
  applyMigration backend bs
  assertEqual "installed_migrations table exists" ["installed_migrations"] =<< getTables conn
  assertEqual "successfully bootstrapped" [mId bs] =<< getMigrations backend

isBootstrappedTrueTest :: (IConnection a) => a -> IO ()
isBootstrappedTrueTest conn = do
  result <- isBootstrapped $ hdbcBackend conn
  assertBool "Bootstrapped check" result

isBootstrappedFalseTest :: (IConnection a) => a -> IO ()
isBootstrappedFalseTest conn = do
  result <- isBootstrapped $ hdbcBackend conn
  assertBool "Bootstrapped check" $ not result

ignoreSqlExceptions :: IO a -> IO (Maybe a)
ignoreSqlExceptions act = (act >>= return . Just) `catchSql`
                       (\_ -> return Nothing)

applyMigrationSuccess :: (IConnection a) => a -> IO ()
applyMigrationSuccess conn = do
    let backend = hdbcBackend conn

    let m1 = (newMigration "validMigration") { mApply = "CREATE TABLE valid1 (a int); CREATE TABLE valid2 (a int);" }

    -- Apply the migrations, ignore exceptions
    withTransaction conn $ \conn' -> applyMigration (hdbcBackend conn') m1

    -- Check that none of the migrations were installed
    assertEqual "Installed migrations" ["root", "validMigration"] =<< getMigrations backend
    assertEqual "Installed tables" ["installed_migrations", "valid1", "valid2"] =<< getTables conn

-- |Does a failure to apply a migration imply a transaction rollback?
applyMigrationFailure :: (IConnection a) => a -> IO ()
applyMigrationFailure conn = do
    let backend = hdbcBackend conn

    let m1 = (newMigration "second") { mApply = "CREATE TABLE validButTemporary (a int)" }
        m2 = (newMigration "third") { mApply = "INVALID SQL" }

    -- Apply the migrations, ignore exceptions
    ignoreSqlExceptions $ withTransaction conn $ \conn' -> do
        let backend' = hdbcBackend conn'
        applyMigration backend' m1
        applyMigration backend' m2

    -- Check that none of the migrations were installed
    assertEqual "Installed migrations" ["root"] =<< getMigrations backend
    assertEqual "Installed tables" ["installed_migrations"] =<< getTables conn

revertMigrationFailure :: (IConnection a) => a -> IO ()
revertMigrationFailure conn = do
    let backend = hdbcBackend conn

    let m1 = (newMigration "second") { mApply = "CREATE TABLE validRMF (a int)"
                                     , mRevert = Just "DROP TABLE validRMF"}
        m2 = (newMigration "third") { mApply = "SELECT * FROM validRMF"
                                    , mRevert = Just "INVALID REVERT SQL"}

    applyMigration backend m1
    applyMigration backend m2

    installedBeforeRevert <- getMigrations backend

    commitBackend backend

    -- Revert the migrations, ignore exceptions; the revert will fail,
    -- but withTransaction will roll back.
    ignoreSqlExceptions $ withTransaction conn $ \conn' -> do
        let backend' = hdbcBackend conn'
        revertMigration backend' m2
        revertMigration backend' m1

    -- Check that none of the migrations were reverted
    assertEqual "successfully roll back failed revert" installedBeforeRevert
        =<< getMigrations backend

revertMigrationNothing :: (IConnection a) => a -> IO ()
revertMigrationNothing conn = do
    let backend = hdbcBackend conn

    let m1 = (newMigration "second") { mApply = "SELECT 1"
                                     , mRevert = Nothing }

    applyMigration backend m1

    installedAfterApply <- getMigrations backend
    assertBool "Check that the migration was applied" $ "second" `elem` installedAfterApply

    -- Revert the migration, which should do nothing EXCEPT remove it
    -- from the installed list
    revertMigration backend m1

    installed <- getMigrations backend
    assertBool "Check that the migration was reverted" $ not $ "second" `elem` installed

revertMigrationJust :: (IConnection a) => a -> IO ()
revertMigrationJust conn = do
    let name = "revertable"
        backend = hdbcBackend conn

    let m1 = (newMigration name) { mApply = "CREATE TABLE the_test_table (a int)"
                                 , mRevert = Just "DROP TABLE the_test_table" }

    applyMigration backend m1

    installedAfterApply <- getMigrations backend
    assertBool "Check that the migration was applied" $ name `elem` installedAfterApply

    -- Revert the migration, which should do nothing EXCEPT remove it
    -- from the installed list
    revertMigration backend m1

    installed <- getMigrations backend
    assertBool "Check that the migration was reverted" $ not $ name `elem` installed

#endif
