{-# LANGUAGE QuasiQuotes #-}

import MPrelude

import qualified CBT
import qualified CBT.Container
import qualified DBT.Postgresql.Connection as DBT
import qualified DBT.Postgresql.Container  as DBT
import qualified Devtools
import qualified Hasql.Connection          as Hasql
import qualified Hasql.Session             as Hasql
import qualified Hasql.Statement           as Hasql
import qualified MHasql.TH                 as MHasql
import qualified Test.Database             as Test
import qualified Test.Tasty                as Tasty
import qualified Test.Tasty.HUnit          as Tasty
import qualified UnliftIO.Exception        as Exception

main :: IO ()
main = do
  CBT.runDefaultEnvironment $ do
    containerName <- CBT.Container.nextName (CBT.Container.Prefix "mhasql-test")
    DBT.withDatabaseContainerDefault containerName $ \clientConfig ->
      DBT.withConnection clientConfig $ \connection ->
        liftIO . Tasty.defaultMain $ Tasty.testGroup "mhasql"
          [ Devtools.testTree $$(Devtools.readDependencies [Devtools.Target "mhasql-th"])
          , testDB connection
          ]

testDB :: Hasql.Connection -> Tasty.TestTree
testDB connection =
  Tasty.testCase "smoke test" $ do
    runSession
      $ Hasql.sql
        [MHasql.uncheckedSql|
          CREATE DOMAIN
            bool_not_null
          AS
            bool
          NOT NULL
        |]

    testStatement (pure True) () [Test.singletonStatement|SELECT true :: bool|]
    testStatement True        () [Test.singletonStatement|SELECT true :: bool_not_null|]
  where
    testStatement expected params statement =
      Tasty.assertEqual "" expected =<< runStatement params statement

    runStatement :: a -> Hasql.Statement a b -> IO b
    runStatement params = runSession . Hasql.statement params

    runSession :: Hasql.Session a -> IO a
    runSession session =
      either Exception.throwIO pure =<< Hasql.run session connection
