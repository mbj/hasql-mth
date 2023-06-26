module Test.Database where

import Language.Haskell.TH.Quote
import MPrelude

import qualified MHasql.TH            as MHasql
import qualified MHasql.TH.Codec      as MHasql
import qualified PostgresqlSyntax.Ast as PG

singletonStatement :: QuasiQuoter
singletonStatement = MHasql.singletonStatement findCodec

findCodec :: PG.SimpleTypename -> Either Text MHasql.Codec
findCodec = MHasql.customFindCodec $ \case
  "bool_not_null" -> pure $ MHasql.setNonNull MHasql.bool
  other           -> MHasql.genericName other
