module FilesystemParseTest
    ( tests
    )
where

import Test.HUnit
import Data.Time.Clock ( UTCTime )
import System.FilePath ( (</>) )

import Common

import Database.Schema.Migrations.Migration
import Database.Schema.Migrations.Filesystem
    ( FilesystemStore(..)
    , migrationFromFile
    )

tests :: IO [Test]
tests = migrationParsingTests

-- filename, result
type MigrationParsingTestCase = (FilePath, Either String Migration)

tsStr :: String
tsStr = "2009-04-15 10:02:06 UTC"

ts :: UTCTime
ts = read tsStr

valid_full :: Migration
valid_full = Migration {
               mTimestamp = ts
             , mId = "valid_full"
             , mDesc = Just "A valid full migration."
             , mDeps = ["another_migration"]
             , mApply = "CREATE TABLE test ( a int );"
             , mRevert = Just "DROP TABLE test;"
             }

testStorePath :: FilePath
testStorePath = testFile $ "migration_parsing"

fp :: FilePath -> FilePath
fp = (testStorePath </>)

migrationParsingTestCases :: [MigrationParsingTestCase]
migrationParsingTestCases = [ ("valid_full", Right valid_full)
                            , ("valid_with_comments"
                              , Right (valid_full { mId = "valid_with_comments" }))
                            , ("valid_with_multiline_deps"
                              , Right (valid_full { mId = "valid_with_multiline_deps"
                                                  , mDeps = ["one", "two", "three"] } ))
                            , ("valid_no_depends"
                              , Right (valid_full { mId = "valid_no_depends", mDeps = [] }))
                            , ("valid_no_desc"
                              , Right (valid_full { mId = "valid_no_desc", mDesc = Nothing }))
                            , ("valid_no_revert"
                              , Right (valid_full { mId = "valid_no_revert", mRevert = Nothing }))
                            , ("invalid_missing_required_fields"
                              , Left $ "AesonException \"key \\\"Created\\\" not present\"")
                            , ("invalid_field_name"
                              , Right (valid_full { mId = "invalid_field_name", mDeps = ["valid"], mDesc = Just "The first migration in the store.", mApply = "CREATE TABLE test (a int);" }))
                            , ("invalid_syntax"
                              , Left $ "InvalidYaml (Just (YamlParseException {yamlProblem = \"could not find expected ':'\", yamlContext = \"while scanning a simple key\", yamlProblemMark = YamlMark {yamlIndex = 130, yamlLine = 6, yamlColumn = 0}}))")
                            , ("invalid_timestamp"
                              , Left $ "AesonException \"could not parse ISO date\"")
                            ]

mkParsingTest :: MigrationParsingTestCase -> IO Test
mkParsingTest (fname, expected) = do
  let store = FSStore { storePath = testStorePath }
  actual <- migrationFromFile store fname
  return $ test $ expected ~=? actual

migrationParsingTests :: IO [Test]
migrationParsingTests =
    sequence $ map mkParsingTest migrationParsingTestCases
