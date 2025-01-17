-- |
-- AST traversal extracting input types.
module Hasql.MTH.Extraction.InputTypeList
  ( preparableStmt
  )
where

import Data.Functor (($>))
import Hasql.MTH.Prelude
import PostgresqlSyntax.Ast

import qualified Data.IntMap.Strict as IntMap
import qualified Hasql.MTH.Extraction.PlaceholderTypeMap as PlaceholderTypeMap

-- |
-- >>> import qualified PostgresqlSyntax.Parsing as P
-- >>> test = either fail (pure . preparableStmt) . P.run P.preparableStmt
--
-- >>> test "select $1 :: INT"
-- Right [Typename False (NumericSimpleTypename IntNumeric) False Nothing]
--
-- >>> test "select $1 :: INT, a + $2 :: INTEGER"
-- Right [Typename False (NumericSimpleTypename IntNumeric) False Nothing,Typename False (NumericSimpleTypename IntegerNumeric) False Nothing]
--
-- >>> test "select $1 :: INT4"
-- Right [Typename False (GenericTypeSimpleTypename (GenericType (UnquotedIdent "int4") Nothing Nothing)) False Nothing]
--
-- >>> test "select $1 :: text[]"
-- Right [Typename False (GenericTypeSimpleTypename (GenericType (UnquotedIdent "text") Nothing Nothing)) False (Just (BoundsTypenameArrayDimensions (Nothing :| []),False))]
--
-- >>> test "select $1"
-- Left "Placeholder $1 misses an explicit typecast"
--
-- >>> test "select $2 :: int4, $1 :: int4, $2 :: int4"
-- Right [Typename False (GenericTypeSimpleTypename (GenericType (UnquotedIdent "int4") Nothing Nothing)) False Nothing,Typename False (GenericTypeSimpleTypename (GenericType (UnquotedIdent "int4") Nothing Nothing)) False Nothing]
--
-- >>> test "select $1 :: int4, $1 :: text"
-- Left "Placeholder $1 has conflicting type annotations"
--
-- >>> test "select $2 :: int4, $2 :: text"
-- Left "Placeholder $2 has conflicting type annotations"
--
-- >>> test "select $3 :: int4, $1 :: int4"
-- Left "You've missed placeholder $2"
preparableStmt :: PreparableStmt -> Either Text [Typename]
preparableStmt = placeholderTypeMap <=< PlaceholderTypeMap.preparableStmt

placeholderTypeMap :: IntMap Typename -> Either Text [Typename]
placeholderTypeMap map' =
  zipWithM
    (\a b -> if a == b then Right () else Left ("You've missed placeholder $" <> showAsText b))
    (IntMap.keys map')
    [1 ..]
  $> IntMap.elems map'
