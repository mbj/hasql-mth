module Test.Database where

import Language.Haskell.TH.Quote
import MPrelude

import qualified Hasql.MTH            as Hasql
import qualified Hasql.MTH.Codec      as Hasql
import qualified PostgresqlSyntax.Ast as PG

singletonStatement :: QuasiQuoter
singletonStatement = Hasql.singletonStatement findCodec

findCodec :: PG.SimpleTypename -> Either Text Hasql.Codec
findCodec = Hasql.customFindCodec $ \case
  "bool_not_null" -> pure $ Hasql.setNonNull Hasql.bool
  other           -> Hasql.genericName other
