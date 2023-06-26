module MHasql.TH.Extraction.Exp
  ( FindCodec
  , foldStatement
  , undecodedStatement
  )
where

import Language.Haskell.TH
import MHasql.TH.Codec (Codec)
import MHasql.TH.Prelude

import qualified Data.Text.Encoding                  as Text
import qualified MHasql.TH.Construction.Exp          as Exp
import qualified MHasql.TH.Extraction.InputTypeList  as InputTypeList
import qualified MHasql.TH.Extraction.OutputTypeList as OutputTypeList
import qualified PostgresqlSyntax.Ast                as Ast

type FindCodec = Ast.SimpleTypename -> Either Text Codec

undecodedStatement :: FindCodec -> (Exp -> Exp) -> Text -> Ast.PreparableStmt -> Either Text Exp
undecodedStatement findCodec decoderProj sql ast = do
  (encoder, rowDecoder) <- codec findCodec ast
  pure (Exp.statement (Exp.byteString $ Text.encodeUtf8 sql) encoder (decoderProj rowDecoder))

foldStatement :: FindCodec -> Text -> Ast.PreparableStmt -> Either Text Exp
foldStatement findCodec sql ast = do
  (encoder, rowDecoder) <- codec findCodec ast
  pure (Exp.foldStatement (Exp.byteString $ Text.encodeUtf8 sql) encoder rowDecoder)

codec :: FindCodec -> Ast.PreparableStmt -> Either Text (Exp, Exp)
codec findCodec ast = (,) <$> paramsEncoder ast <*> rowDecoder ast
  where
    paramsEncoder :: Ast.PreparableStmt -> Either Text Exp
    paramsEncoder = fmap Exp.contrazip . traverse paramEncoder <=< InputTypeList.preparableStmt

    paramEncoder :: Ast.Typename -> Either Text Exp
    paramEncoder =
      byTypename
        findCodec
        Exp.unidimensionalParamEncoder
        Exp.multidimensionalParamEncoder

    rowDecoder :: Ast.PreparableStmt -> Either Text Exp
    rowDecoder = fmap Exp.cozip . traverse columnDecoder <=< OutputTypeList.preparableStmt

    columnDecoder :: Ast.Typename -> Either Text Exp
    columnDecoder =
      byTypename
        findCodec
        Exp.unidimensionalColumnDecoder
        Exp.multidimensionalColumnDecoder

byTypename
  :: FindCodec
  -> (Codec -> Exp)
  -> (Codec -> Int -> Exp)
  -> Ast.Typename -> Either Text Exp
byTypename findCodec unidimensional multidimensional (Ast.Typename setof simpleTypename dimensions) =
  if setof
    then Left "SETOF is not supported"
    else do
      codec' <- findCodec simpleTypename
      pure $ maybe (unidimensional codec') (mkMultidimensional codec') dimensions
  where
    mkMultidimensional codec' = \case
      Ast.BoundsTypenameArrayDimensions h   -> multidimensional codec' (length h)
      Ast.ExplicitTypenameArrayDimensions _ -> multidimensional codec' 1
