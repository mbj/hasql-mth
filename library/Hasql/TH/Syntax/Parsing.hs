{-|

Our parsing strategy is to port the original Postgres parser as closely as possible.

We're using the @gram.y@ Postgres source file, which is the closest thing we have
to a Postgres syntax spec. Here's a link to it:
https://github.com/postgres/postgres/blob/master/src/backend/parser/gram.y.

Here's the essence of how the original parser is implemented, citing from
[PostgreSQL Wiki](https://wiki.postgresql.org/wiki/Developer_FAQ):

    scan.l defines the lexer, i.e. the algorithm that splits a string
    (containing an SQL statement) into a stream of tokens.
    A token is usually a single word
    (i.e., doesn't contain spaces but is delimited by spaces), 
    but can also be a whole single or double-quoted string for example. 
    The lexer is basically defined in terms of regular expressions 
    which describe the different token types.

    gram.y defines the grammar (the syntactical structure) of SQL statements,
    using the tokens generated by the lexer as basic building blocks.
    The grammar is defined in BNF notation.
    BNF resembles regular expressions but works on the level of tokens, not characters.
    Also, patterns (called rules or productions in BNF) are named, and may be recursive,
    i.e. use themselves as sub-patterns.

-}
module Hasql.TH.Syntax.Parsing where

import Hasql.TH.Prelude hiding (aExpr, try, option, some, many, sortBy, filter)
import Text.Megaparsec hiding (some, endBy1, someTill, sepBy1, sepEndBy1)
import Text.Megaparsec.Char
import Control.Applicative.Combinators.NonEmpty
import Hasql.TH.Syntax.Ast
import qualified Text.Megaparsec.Char.Lexer as Lex
import qualified Hasql.TH.Syntax.Predicate as Predicate
import qualified Hasql.TH.Syntax.HashSet as HashSet
import qualified Data.Text as Text
import qualified Text.Builder as TextBuilder


{- $setup
>>> testParser parser = parseTest (parser <* eof)
-}


type Parser = Parsec Void Text


-- * Helpers
-------------------------

filter :: Text -> (a -> Bool) -> Parser a -> Parser a
filter _error _predicate _parser = try $ do
  _result <- _parser
  if _predicate _result
    then return _result
    else fail (Text.unpack _error)

commaSeparator :: Parser ()
commaSeparator = try $ space *> char ',' *> space

dotSeparator :: Parser ()
dotSeparator = try $ space *> char '.' *> space

inParens :: Parser a -> Parser a
inParens p = try (char '(' *> space) *> p <* space <* char ')'

inParensWithLabel :: (label -> content -> result) -> Parser label -> Parser content -> Parser result
inParensWithLabel _result _labelParser _contentParser = do
  _label <- try $ _labelParser <* space <* char '('
  _content <- _contentParser
  space
  char ')'
  return (_result _label _content)

inParensWithClause :: Parser clause -> Parser content -> Parser content
inParensWithClause = inParensWithLabel (const id)

nonEmptyList :: Parser a -> Parser (NonEmpty a)
nonEmptyList p = sepBy1 p commaSeparator

sepWithSpace1 :: Parser a -> Parser (NonEmpty a)
sepWithSpace1 _parser = do
  _head <- _parser
  _tail <- many $ try (space1 *> _parser)
  return (_head :| _tail)

{-|
>>> testParser (quotedString '\'') "'abc''d'"
"abc'd"
-}
quotedString :: Char -> Parser Text
quotedString q = do
  try $ char q
  let
    collectChunks !bdr = do
      chunk <- takeWhileP Nothing (/= q)
      let bdr' = bdr <> TextBuilder.text chunk
      try (consumeEscapedQuote bdr') <|> finish bdr'
    consumeEscapedQuote bdr = do
      char q
      char q
      collectChunks (bdr <> TextBuilder.char q)
    finish bdr = do
      char q
      return (TextBuilder.run bdr)
    in collectChunks mempty

quasiQuote :: Parser a -> Parser a
quasiQuote p = try $ space *> p <* space <* eof


-- * PreparableStmt
-------------------------

preparableStmt :: Parser PreparableStmt
preparableStmt = selectPreparableStmt

selectPreparableStmt :: Parser PreparableStmt
selectPreparableStmt = SelectPreparableStmt <$> selectStmt


-- * Select
-------------------------

selectStmt :: Parser SelectStmt
selectStmt = inParensSelectStmt <|> noParensSelectStmt

inParensSelectStmt :: Parser SelectStmt
inParensSelectStmt = inParens (InParensSelectStmt <$> selectStmt)

noParensSelectStmt :: Parser SelectStmt
noParensSelectStmt = NoParensSelectStmt <$> selectNoParens

selectWithParens = inParens (selectNoParens <|> selectWithParens)

selectNoParens :: Parser SelectNoParens
selectNoParens = simpleSelectNoParens <|> withSelectNoParens

sharedSelectNoParens _with = do
  _select <- selectClause
  asum
    [
      do
        try space1
        asum [sort _select, limit _select Nothing, forLocking _select Nothing Nothing]
      ,
      pure (SelectNoParens _with _select Nothing Nothing Nothing)
    ]
  where
    sort _select = 
      (do
        _sort <- sortClause
        limit _select (Just _sort)
      ) <|>
      limit _select Nothing
    limit _select _optSorting =
      (do
        _limit <- selectLimit
        forLocking _select _optSorting (Just _limit)
      ) <|>
      forLocking _select _optSorting Nothing
    forLocking _select _optSorting _optLimit = do
      _optForLocking <- optional (try (space1 *> forLockingClause))
      return (SelectNoParens _with _select _optSorting _optLimit _optForLocking)

{-|
The one that doesn't start with \"WITH\".
-}
{-
  | simple_select
  | select_clause sort_clause
  | select_clause opt_sort_clause for_locking_clause opt_select_limit
  | select_clause opt_sort_clause select_limit opt_for_locking_clause
-}
simpleSelectNoParens = sharedSelectNoParens Nothing

withSelectNoParens = do
  _with <- withClause
  space1
  sharedSelectNoParens (Just _with)

selectClause =
  asum
    [
      Right <$> selectWithParens,
      Left <$> simpleSelect
    ]

{-|
>>> test = testParser simpleSelect

>>> test "select"
NormalSimpleSelect Nothing Nothing Nothing Nothing Nothing Nothing Nothing

>>> test "select distinct 1"
...DistinctTargeting Nothing (ExprTarget (LiteralExpr (IntLiteral 1)) Nothing :| [])...

>>> test "select $1"
...NormalTargeting (ExprTarget (PlaceholderExpr 1) Nothing :| [])...

>>> test "select $1 + $2"
...BinOpExpr "+" (PlaceholderExpr 1) (PlaceholderExpr 2)...

>>> test "select a, b"
...ExprTarget (QualifiedNameExpr (SimpleQualifiedName (UnquotedName "a"))) Nothing :| [ExprTarget (QualifiedNameExpr (SimpleQualifiedName (UnquotedName "b"))) Nothing]...

>>> test "select $1 :: text"
...TypecastExpr (PlaceholderExpr 1) (Type "text" False 0 False)...

>>> test "select 1"
...ExprTarget (LiteralExpr (IntLiteral 1))...

>>> test "select id"
...ExprTarget (QualifiedNameExpr (SimpleQualifiedName (UnquotedName "id"))) Nothing...

>>> test "select id from user"
1:20:
  |
1 | select id from user
  |                    ^
Reserved keyword "user" used as an identifier. If that's what you intend, you have to wrap it in double quotes.

>>> test "select id :: int4 from \"user\""
...TypecastExpr (QualifiedNameExpr (SimpleQualifiedName (UnquotedName "id"))) (Type "int4" False 0 False)...
-}
{-
simple_select:
  |  SELECT opt_all_clause opt_target_list
      into_clause from_clause where_clause
      group_clause having_clause window_clause
  |  SELECT distinct_clause target_list
      into_clause from_clause where_clause
      group_clause having_clause window_clause
  |  values_clause
  |  TABLE relation_expr
  |  select_clause UNION all_or_distinct select_clause
  |  select_clause INTERSECT all_or_distinct select_clause
  |  select_clause EXCEPT all_or_distinct select_clause

TODO: handle remaining cases
-}
simpleSelect :: Parser SimpleSelect
simpleSelect = asum
  [
    do
      try $ do
        string' "select"
        notFollowedBy $ satisfy $ isAlphaNum
      _targeting <- optional (try (space1 *> targeting))
      _intoClause <- optional (try (space1 *> string' "into" *> space1) *> optTempTableName)
      _fromClause <- optional (try (space1 *> string' "from" *> space1) *> nonEmptyList tableRef)
      _whereClause <- optional (try (space1 *> string' "where" *> space1) *> aExpr)
      _groupClause <- optional (try (space1 *> keyphrase "group by" *> space1) *> nonEmptyList groupByItem)
      _havingClause <- optional (try (space1 *> string' "having" *> space1) *> aExpr)
      _windowClause <- optional (try (space1 *> string' "window" *> space1) *> nonEmptyList windowDefinition)
      return (NormalSimpleSelect _targeting _intoClause _fromClause _whereClause _groupClause _havingClause _windowClause)
  ]

withClause = label "with clause" $ do
  try $ do
    string' "with"
    space1
  _recursive <- option False (True <$ try (string' "recursive" <* space1))
  _cteList <- nonEmptyList commonTableExpr
  return (WithClause _recursive _cteList)

commonTableExpr = label "common table expression" $ do
  _name <- try (colId <* space)
  _nameList <- optional (inParens (nonEmptyList colId) <* space1)
  string' "as"
  space1
  _materialized <- optional (materialized <* space1)
  _stmt <- inParens preparableStmt
  return (CommonTableExpr _name _nameList _materialized _stmt)

materialized = label "materialization" $
  True <$ try (string' "materialized") <|>
  False <$ try (keyphrase "not materialized")

{-
simple_select:
  |  SELECT opt_all_clause opt_target_list
      into_clause from_clause where_clause
      group_clause having_clause window_clause
  |  SELECT distinct_clause target_list
      into_clause from_clause where_clause
      group_clause having_clause window_clause

distinct_clause:
  |  DISTINCT
  |  DISTINCT ON '(' expr_list ')'
-}
targeting :: Parser Targeting
targeting = distinct <|> allWithTargetList <|> all <|> normal <?> "targeting" where
  normal = NormalTargeting <$> targetList
  allWithTargetList = do
    try $ do
      string' "all"
      space1
    AllTargeting <$> Just <$> targetList
  all = try $ string' "all" $> AllTargeting Nothing
  distinct = do
    try $ do
      string' "distinct"
      space1
    _optOn <- optional (try (onExpressionsClause <* space1))
    _targetList <- targetList
    return (DistinctTargeting _optOn _targetList)

targetList :: Parser (NonEmpty Target)
targetList = nonEmptyList target

{-|
>>> testParser target "a.b as c"
ExprTarget (QualifiedNameExpr (IndirectedQualifiedName (UnquotedName "a") (AttrNameIndirectionEl (UnquotedName "b") :| []))) (Just (UnquotedName "c"))
-}
{-
target_el:
  |  a_expr AS ColLabel
  |  a_expr IDENT
  |  a_expr
  |  '*'
-}
target :: Parser Target
target = exprCase <|> allCase <?> "target" where
  allCase = AllTarget <$ try (char '*')
  exprCase = do
    _expr <- aExpr
    _optAlias <- optional $ asum
      [
        do
          try $ do
            space1
            string' "as"
            space1
          colLabel
        ,
        try (space1 *> ident)
      ]
    return (ExprTarget _expr _optAlias)

onExpressionsClause :: Parser (NonEmpty Expr)
onExpressionsClause = do
  try $ do
    string' "on"
    space1
  nonEmptyList aExpr


-- * Into clause details
-------------------------

optTempTableName :: Parser OptTempTableName
optTempTableName = error "TODO"


-- * Group by details
-------------------------

groupByItem :: Parser GroupByItem
groupByItem = error "TODO"


-- * Window clause details
-------------------------

windowDefinition :: Parser WindowDefinition
windowDefinition = error "TODO"


-- * Table refs
-------------------------

{-|
>>> testParser tableRef "a left join b "
...
expecting "on", "using", or white space

>>> testParser tableRef "a left join b on (a.i = b.i)"
JoinTableRef (MethJoinedTable (QualJoinMeth...

-}
{-
| relation_expr opt_alias_clause
| relation_expr opt_alias_clause tablesample_clause
| func_table func_alias_clause
| LATERAL_P func_table func_alias_clause
| xmltable opt_alias_clause
| LATERAL_P xmltable opt_alias_clause
| select_with_parens opt_alias_clause
| LATERAL_P select_with_parens opt_alias_clause
| joined_table
| '(' joined_table ')' alias_clause

TODO: Add support for missing cases.
-}
tableRef :: Parser TableRef
tableRef = label "table reference" $ do
  _tr <- nonTrailingTableRef
  trailingTableRef _tr <|> pure _tr
  where
    
    nonTrailingTableRef = asum [
        relationExprTableRef <|>
        lateralTableRef <|> nonLateralTableRef <|>
        inParensJoinedTableTableRef <|>
        joinedTableWithAliasTableRef
      ]
      where
        
        {-
        | relation_expr opt_alias_clause
        | relation_expr opt_alias_clause tablesample_clause

        TODO: Add support for TABLESAMPLE.
        -}
        relationExprTableRef = do
          _relationExpr <- try $ relationExpr
          _optAliasClause <- optional $ try $ space1 *> aliasClause
          return (RelationExprTableRef _relationExpr _optAliasClause)

        {-
        | LATERAL_P func_table func_alias_clause
        | LATERAL_P xmltable opt_alias_clause
        | LATERAL_P select_with_parens opt_alias_clause
        -}
        lateralTableRef = do
          try $ do
            string' "lateral"
            space1
          selectWithParensTableRef True

        nonLateralTableRef = selectWithParensTableRef False

        selectWithParensTableRef _lateral = do
          _select <- selectWithParens
          _optAliasClause <- optional $ try $ space1 *> aliasClause
          return (SelectTableRef _lateral _select _optAliasClause)

        inParensJoinedTableTableRef = JoinTableRef <$> inParensJoinedTable <*> pure Nothing

        joinedTableWithAliasTableRef = do
          _joinedTable <- inParens joinedTable
          space1
          _alias <- aliasClause
          return (JoinTableRef _joinedTable (Just _alias))

    trailingTableRef _tableRef =
      JoinTableRef <$> tableRefJoinedTableAfterSpace _tableRef <*> pure Nothing

{-
| qualified_name
| qualified_name '*'
| ONLY qualified_name
| ONLY '(' qualified_name ')'
-}
relationExpr :: Parser RelationExpr
relationExpr =
  label "relation expression" $
  asum
    [
      do
        _name <- qualifiedName
        _asterisk <- asum
          [
            True <$ try (space1 *> char '*'),
            pure False
          ]
        return (SimpleRelationExpr _name _asterisk)
      ,
      inParensWithClause (string' "only") qualifiedName <&> \ a -> OnlyRelationExpr a True
      ,
      do
        try $ do
          string' "only"
          space1
        _name <- qualifiedName
        return (OnlyRelationExpr _name False) 
    ]

joinedTable =
  asum [
      inParensJoinedTable,
      do
        _tr1 <- tableRef
        tableRefJoinedTableAfterSpace _tr1
    ]

{-
  | '(' joined_table ')'
-}
inParensJoinedTable = InParensJoinedTable <$> inParens joinedTable

{-
  | table_ref CROSS JOIN table_ref
  | table_ref join_type JOIN table_ref join_qual
  | table_ref JOIN table_ref join_qual
  | table_ref NATURAL join_type JOIN table_ref
  | table_ref NATURAL JOIN table_ref
-}
tableRefJoinedTableAfterSpace _tr1 = asum $
  [
    do
      try $ space1 *> keyphrase "cross join"
      space1
      _tr2 <- tableRef
      return (MethJoinedTable CrossJoinMeth _tr1 _tr2)
    ,
    do
      _jt <- joinTypedJoinAfterSpace
      space1
      _tr2 <- tableRef
      space1
      _jq <- joinQual
      return (MethJoinedTable (QualJoinMeth _jt _jq) _tr1 _tr2)
    ,
    do
      try $ space1 *> string' "natural"
      _jt <- joinTypedJoinAfterSpace
      space1
      _tr2 <- tableRef
      return (MethJoinedTable (NaturalJoinMeth _jt) _tr1 _tr2)
  ]
  where
    joinTypedJoinAfterSpace =
      Just <$> (try (space1 *> joinType <* space1) <* string' "join") <|>
      Nothing <$ try (space1 *> string' "join")

joinType = asum [
    do
      string' "full"
      _outer <- outerAfterSpace
      return (FullJoinType _outer)
    ,
    do
      string' "left"
      _outer <- outerAfterSpace
      return (LeftJoinType _outer)
    ,
    do
      string' "right"
      _outer <- outerAfterSpace
      return (RightJoinType _outer)
    ,
    string' "inner" $> InnerJoinType
  ]
  where
    outerAfterSpace = try (space1 *> string' "outer") $> True <|> pure False

joinQual = asum
  [
    string' "using" *> space1 *> inParens (nonEmptyList colId) <&> UsingJoinQual
    ,
    string' "on" *> space1 *> aExpr <&> OnJoinQual
  ]

{-
alias_clause:
  |  AS ColId '(' name_list ')'
  |  AS ColId
  |  ColId '(' name_list ')'
  |  ColId
name_list:
  |  name
  |  name_list ',' name
name:
  |  ColId
-}
aliasClause :: Parser AliasClause
aliasClause = do
  _alias <- (try (string' "as" *> space1) *> colId) <|> try colId
  _columnAliases <- optional $ try $ space1 *> inParens (nonEmptyList colId)
  return (AliasClause _alias _columnAliases)


-- * Expressions
-------------------------
{-

a_expr:
  | c_expr
  | a_expr TYPECAST Typename
  | a_expr COLLATE any_name
  | a_expr AT TIME ZONE a_expr
  | '+' a_expr
  | '-' a_expr
  | a_expr '+' a_expr
  | a_expr '-' a_expr
  | a_expr '*' a_expr
  | a_expr '/' a_expr
  | a_expr '%' a_expr
  | a_expr '^' a_expr
  | a_expr '<' a_expr
  | a_expr '>' a_expr
  | a_expr '=' a_expr
  | a_expr LESS_EQUALS a_expr
  | a_expr GREATER_EQUALS a_expr
  | a_expr NOT_EQUALS a_expr
  | a_expr qual_Op a_expr
  | qual_Op a_expr
  | a_expr qual_Op
  | a_expr AND a_expr
  | a_expr OR a_expr
  | NOT a_expr
  | NOT_LA a_expr
  | a_expr LIKE a_expr
  | a_expr LIKE a_expr ESCAPE a_expr
  | a_expr NOT_LA LIKE a_expr
  | a_expr NOT_LA LIKE a_expr ESCAPE a_expr
  | a_expr ILIKE a_expr
  | a_expr ILIKE a_expr ESCAPE a_expr
  | a_expr NOT_LA ILIKE a_expr
  | a_expr NOT_LA ILIKE a_expr ESCAPE a_expr
  | a_expr SIMILAR TO a_expr
  | a_expr SIMILAR TO a_expr ESCAPE a_expr
  | a_expr NOT_LA SIMILAR TO a_expr
  | a_expr NOT_LA SIMILAR TO a_expr ESCAPE a_expr
  | a_expr IS NULL_P
  | a_expr ISNULL
  | a_expr IS NOT NULL_P
  | a_expr NOTNULL
  | row OVERLAPS row
  | a_expr IS TRUE_P
  | a_expr IS NOT TRUE_P
  | a_expr IS FALSE_P
  | a_expr IS NOT FALSE_P
  | a_expr IS UNKNOWN
  | a_expr IS NOT UNKNOWN
  | a_expr IS DISTINCT FROM a_expr
  | a_expr IS NOT DISTINCT FROM a_expr
  | a_expr IS OF '(' type_list ')'
  | a_expr IS NOT OF '(' type_list ')'
  | a_expr BETWEEN opt_asymmetric b_expr AND a_expr
  | a_expr NOT_LA BETWEEN opt_asymmetric b_expr AND a_expr
  | a_expr BETWEEN SYMMETRIC b_expr AND a_expr
  | a_expr NOT_LA BETWEEN SYMMETRIC b_expr AND a_expr
  | a_expr IN_P in_expr
  | a_expr NOT_LA IN_P in_expr
  | a_expr subquery_Op sub_type select_with_parens
  | a_expr subquery_Op sub_type '(' a_expr ')'
  | UNIQUE select_with_parens
  | a_expr IS DOCUMENT_P
  | a_expr IS NOT DOCUMENT_P
  | DEFAULT

b_expr:
  | c_expr
  | b_expr TYPECAST Typename
  | '+' b_expr
  | '-' b_expr
  | b_expr '+' b_expr
  | b_expr '-' b_expr
  | b_expr '*' b_expr
  | b_expr '/' b_expr
  | b_expr '%' b_expr
  | b_expr '^' b_expr
  | b_expr '<' b_expr
  | b_expr '>' b_expr
  | b_expr '=' b_expr
  | b_expr LESS_EQUALS b_expr
  | b_expr GREATER_EQUALS b_expr
  | b_expr NOT_EQUALS b_expr
  | b_expr qual_Op b_expr
  | qual_Op b_expr
  | b_expr qual_Op
  | b_expr IS DISTINCT FROM b_expr
  | b_expr IS NOT DISTINCT FROM b_expr
  | b_expr IS OF '(' type_list ')'
  | b_expr IS NOT OF '(' type_list ')'
  | b_expr IS DOCUMENT_P
  | b_expr IS NOT DOCUMENT_P

-}

{-|
Notice that the tree constructed by this parser does not reflect
the precedence order of Postgres.
For the purposes of this library it simply doesn't matter,
so we're not bothering with that.

Composite on the right:
>>> testParser aExpr "$1 = $2 :: int4"
BinOpExpr "=" (PlaceholderExpr 1) (TypecastExpr (PlaceholderExpr 2) (Type "int4" False 0 False))

Composite on the left:
>>> testParser aExpr "$1 = $2 :: int4 and $3"
BinOpExpr "=" (PlaceholderExpr 1) (BinOpExpr "and" (TypecastExpr (PlaceholderExpr 2) (Type "int4" False 0 False)) (PlaceholderExpr 3))
-}
aExpr :: Parser Expr
aExpr = label "expression" $ do
  _left <- nonLoopingExpr
  loopingExpr _left <|> pure _left

{-
c_expr:
  | columnref
  | AexprConst
  | PARAM opt_indirection
  | '(' a_expr ')' opt_indirection
  | case_expr
  | func_expr
  | select_with_parens
  | select_with_parens indirection
  | EXISTS select_with_parens
  | ARRAY select_with_parens
  | ARRAY array_expr
  | explicit_row
  | implicit_row
  | GROUPING '(' expr_list ')'

TODO: Add missing cases.
-}
cExpr :: Parser Expr
cExpr =
  asum
    [
      placeholderExpr,
      columnRefExpr,
      literalExpr,
      inParensExpr,
      caseExpr,
      funcExpr,
      selectExpr,
      existsSelectExpr,
      arraySelectExpr,
      groupingExpr
    ]

loopingExpr :: Expr -> Parser Expr
loopingExpr _left = do
  _expr <- asum
    [
      typecastExpr _left,
      binOpExpr _left,
      escapableBinOpExpr _left
    ]
  loopingExpr _expr <|> pure _expr

nonLoopingExpr :: Parser Expr
nonLoopingExpr =
  asum
    [
      defaultExpr,
      placeholderExpr,
      columnRefExpr,
      literalExpr,
      inParensExpr,
      caseExpr,
      funcExpr,
      selectExpr,
      existsSelectExpr,
      arraySelectExpr,
      groupingExpr
    ]

placeholderExpr :: Parser Expr
placeholderExpr = PlaceholderExpr <$> (try (char '$') *> Lex.decimal)

inParensExpr :: Parser Expr
inParensExpr = InParensExpr <$> inParens aExpr <*> optional (try (space1 *> indirection))

typecastExpr :: Expr -> Parser Expr
typecastExpr _left = do
  try $ do
    space
    string "::"
  space
  _type <- type_
  return (TypecastExpr _left _type)

binOpExpr :: Expr -> Parser Expr
binOpExpr _a = do
  _binOp <- try (space *> symbolicBinOp <* space) <|> try (space1 *> lexicalBinOp <* space1)
  _b <- aExpr
  return (BinOpExpr _binOp _a _b)

symbolicBinOp :: Parser Text
symbolicBinOp = try $ do
  _text <- takeWhile1P Nothing Predicate.symbolicBinOpChar
  if Predicate.inSet HashSet.symbolicBinOp _text
    then return _text
    else fail ("Unknown binary operator: " <> show _text)

lexicalBinOp :: Parser Text
lexicalBinOp = asum $ fmap keyphrase $ ["and", "or", "is distinct from", "is not distinct from"]

escapableBinOpExpr :: Expr -> Parser Expr
escapableBinOpExpr _a = do
  (_not, _op) <- try $ do
    space1
    _not <- option False $ try $ True <$ string' "not" <* space1
    _op <- asum $ fmap keyphrase $ ["like", "ilike", "similar to"]
    return (_not, _op)
  space1
  _b <- aExpr
  _escaping <- optional $ try $ do
    string' "escape"
    space1
    aExpr
  return (EscapableBinOpExpr _not _op _a _b _escaping)

defaultExpr :: Parser Expr
defaultExpr = DefaultExpr <$ string' "default"

columnRefExpr :: Parser Expr
columnRefExpr = QualifiedNameExpr <$> columnRef

literalExpr :: Parser Expr
literalExpr = LiteralExpr <$> literal

{-|
Full specification:

>>> testParser caseExpr "CASE WHEN a = b THEN c WHEN d THEN e ELSE f END"
CaseExpr Nothing (WhenClause (BinOpExpr "=" (QualifiedNameExpr (SimpleQualifiedName (UnquotedName "a"))) (QualifiedNameExpr (SimpleQualifiedName (UnquotedName "b")))) (QualifiedNameExpr (SimpleQualifiedName (UnquotedName "c"))) :| [WhenClause (QualifiedNameExpr (SimpleQualifiedName (UnquotedName "d"))) (QualifiedNameExpr (SimpleQualifiedName (UnquotedName "e")))]) (Just (QualifiedNameExpr (SimpleQualifiedName (UnquotedName "f"))))

Implicit argument:

>>> testParser caseExpr "CASE a WHEN b THEN c ELSE d END"
CaseExpr (Just (QualifiedNameExpr (SimpleQualifiedName (UnquotedName "a")))) (WhenClause (QualifiedNameExpr (SimpleQualifiedName (UnquotedName "b"))) (QualifiedNameExpr (SimpleQualifiedName (UnquotedName "c"))) :| []) (Just (QualifiedNameExpr (SimpleQualifiedName (UnquotedName "d"))))
-}
caseExpr :: Parser Expr
caseExpr = label "case expression" $ do
  try $ do
    string' "case"
    space1
  (_arg, _whenClauses) <-
    (Nothing,) <$> sepWithSpace1 whenClause <|>
    (,) <$> (Just <$> aExpr <* space1) <*> sepWithSpace1 whenClause
  space1
  _default <- optional $ do
    try $ do
      string' "else"
      space1
    aExpr <* space1
  string' "end"
  return $ CaseExpr _arg _whenClauses _default

whenClause :: Parser WhenClause
whenClause = do
  try $ do
    string' "when"
    space1
  _a <- aExpr
  space1
  string' "then"
  space1
  _b <- aExpr
  return (WhenClause _a _b)

funcExpr :: Parser Expr
funcExpr = FuncExpr <$> funcApplication

funcApplication :: Parser FuncApplication
funcApplication = inParensWithLabel FuncApplication funcName (optional (try funcApplicationParams))

funcApplicationParams :: Parser FuncApplicationParams
funcApplicationParams =
  asum
    [
      normalFuncApplicationParams,
      singleVariadicFuncApplicationParams,
      listVariadicFuncApplicationParams
    ]

normalFuncApplicationParams :: Parser FuncApplicationParams
normalFuncApplicationParams = try $ do
  _optAllOrDistinct <- optional ((string' "all" $> AllAllOrDistinct <|> string' "distinct" $> DistinctAllOrDistinct) <* space1)
  _argList <- nonEmptyList funcArgExpr
  _optSortClause <- optional (space1 *> sortClause)
  return (NormalFuncApplicationParams _optAllOrDistinct _argList _optSortClause)

singleVariadicFuncApplicationParams :: Parser FuncApplicationParams
singleVariadicFuncApplicationParams = try $ do
  string' "variadic"
  space1
  _arg <- funcArgExpr
  _optSortClause <- optional (space1 *> sortClause)
  return (VariadicFuncApplicationParams Nothing _arg _optSortClause)

listVariadicFuncApplicationParams :: Parser FuncApplicationParams
listVariadicFuncApplicationParams = try $ do
  _argList <- nonEmptyList funcArgExpr
  commaSeparator
  string' "variadic"
  space1
  _arg <- funcArgExpr
  _optSortClause <- optional (space1 *> sortClause)
  return (VariadicFuncApplicationParams (Just _argList) _arg _optSortClause)

funcArgExpr :: Parser FuncArgExpr
funcArgExpr = ExprFuncArgExpr <$> aExpr

sortClause :: Parser (NonEmpty SortBy)
sortClause = do
  try $ do
    keyphrase "order by"
    space1
  nonEmptyList sortBy

sortBy :: Parser SortBy
sortBy = do
  _expr <- try aExpr
  _optOrder <- optional (try (space1 *> order))
  return (SortBy _expr _optOrder)

order :: Parser Order
order = string' "asc" $> AscOrder <|> string' "desc" $> DescOrder

selectExpr :: Parser Expr
selectExpr = InParensExpr <$> (SelectExpr <$> inParens selectNoParens) <*> optional (try (space1 *> indirection))

existsSelectExpr :: Parser Expr
existsSelectExpr = inParensWithClause (string' "array") (ExistsSelectExpr <$> selectNoParens)

arraySelectExpr :: Parser Expr
arraySelectExpr = inParensWithClause (string' "array") (ArraySelectExpr <$> selectNoParens)

groupingExpr :: Parser Expr
groupingExpr = inParensWithClause (string' "grouping") (GroupingExpr <$> nonEmptyList aExpr)


-- * Literals
-------------------------

{-|
@
AexprConst: Iconst
      | FCONST
      | Sconst
      | BCONST
      | XCONST
      | func_name Sconst
      | func_name '(' func_arg_list opt_sort_clause ')' Sconst
      | ConstTypename Sconst
      | ConstInterval Sconst opt_interval
      | ConstInterval '(' Iconst ')' Sconst
      | TRUE_P
      | FALSE_P
      | NULL_P
@

>>> testParser literal "- 324098320984320480392480923842"
IntLiteral (-324098320984320480392480923842)

>>> testParser literal "'abc''de'"
StringLiteral "abc'de"

>>> testParser literal "23.43234"
FloatLiteral 23.43234

>>> testParser literal "-32423423.3243248732492739847923874"
FloatLiteral -3.24234233243248732492739847923874e7

>>> testParser literal "NULL"
NullLiteral
-}
literal :: Parser Literal
literal = label "literal" $ asum [numericLiteral, stringLiteral, boolLiteral, nullLiteral]

numericLiteral :: Parser Literal
numericLiteral = label "numeric literal" $ fmap (either IntLiteral FloatLiteral) $ intOrFloat

stringLiteral :: Parser Literal
stringLiteral = quotedString '\'' <&> StringLiteral <?> "string literal"

boolLiteral :: Parser Literal
boolLiteral = BoolLiteral True <$ string' "true" <|> BoolLiteral False <$ string' "false" <?> "bool literal"

nullLiteral :: Parser Literal
nullLiteral = NullLiteral <$ string' "null" <?> "null literal"

intOrFloat = label "int or float" $ try $ do
  (_input, _scientific) <- match $ Lex.signed space Lex.scientific
  case parseMaybe (Lex.signed space Lex.decimal <* eof :: Parser Integer) _input of
    Just _int -> return (Left _int)
    Nothing -> return (Right _scientific)


-- * Types
-------------------------

{-|
>>> testParser type_ "int4"
Type "int4" False 0 False

>>> testParser type_ "int4?"
Type "int4" True 0 False

>>> testParser type_ "int4[]"
Type "int4" False 1 False

>>> testParser type_ "int4[ ] []"
Type "int4" False 2 False

>>> testParser type_ "int4[][]?"
Type "int4" False 2 True

>>> testParser type_ "int4?[][]"
Type "int4" True 2 False
-}
type_ :: Parser Type
type_ = do
  _baseName <- try $ fmap Text.toLower $ takeWhile1P Nothing isAlphaNum
  _baseNullable <- option False (try (True <$ space <* char '?'))
  _arrayLevels <- fmap length $ many $ try (space *> char '[' *> space) *> char ']'
  _arrayNullable <- option False (try (True <$ space <* char '?'))
  return (Type _baseName _baseNullable _arrayLevels _arrayNullable)


-- * Clauses
-------------------------

{-
select_limit:
  | limit_clause offset_clause
  | offset_clause limit_clause
  | limit_clause
  | offset_clause
-}
selectLimit =
  asum
    [
      do
        _a <- limitClause
        LimitOffsetSelectLimit _a <$> try (space1 *> offsetClause) <|> pure (LimitSelectLimit _a)
      ,
      do
        _a <- offsetClause
        OffsetLimitSelectLimit _a <$> try (space1 *> limitClause) <|> pure (OffsetSelectLimit _a)
    ]

{-
limit_clause:
  | LIMIT select_limit_value
  | LIMIT select_limit_value ',' select_offset_value
  | FETCH first_or_next select_fetch_first_value row_or_rows ONLY
  | FETCH first_or_next row_or_rows ONLY
-}
limitClause =
  (do
    try $ do
      string' "limit"
      space1
    _a <- selectLimitValue
    _b <- optional $ do
      commaSeparator
      aExpr
    return (LimitLimitClause _a _b)
  ) <|>
  (do
    try $ do
      string' "fetch"
      space1
    _a <- firstOrNext
    space1
    _b <- optional (selectFetchFirstValue <* space1)
    _c <- rowOrRows
    space1
    string' "only"
    return (FetchOnlyLimitClause _a _b _c)
  )

offsetClause = do
  try $ do
    string' "offset"
    space1
  offsetClauseParams

offsetClauseParams =
  ExprOffsetClause <$> aExpr <|>
  FetchFirstOffsetClause <$> selectFetchFirstValue <*> (space1 *> rowOrRows)

{-
select_limit_value:
  | a_expr
  | ALL
-}
selectLimitValue =
  ExprSelectLimitValue <$> aExpr <|>
  AllSelectLimitValue <$ string' "all"

rowOrRows =
  True <$ try (string' "rows") <|>
  False <$ try (string' "row")

firstOrNext =
  False <$ try (string' "first") <|>
  True <$ try (string' "next")

selectFetchFirstValue =
  ExprSelectFetchFirstValue <$> cExpr <|>
  NumSelectFetchFirstValue <$> try (plusOrMinus <* space) <*> intOrFloat

plusOrMinus = try $ False <$ char '+' <|> True <$ char '-'


-- * For Locking
-------------------------

{-
for_locking_clause:
  | for_locking_items
  | FOR READ ONLY
for_locking_items:
  | for_locking_item
  | for_locking_items for_locking_item
-}
forLockingClause = readOnly <|> items where
  readOnly = ReadOnlyForLockingClause <$ keyphrase "for read only"
  items = ItemsForLockingClause <$> sepWithSpace1 forLockingItem

{-
for_locking_item:
  | for_locking_strength locked_rels_list opt_nowait_or_skip
locked_rels_list:
  | OF qualified_name_list
  | EMPTY
opt_nowait_or_skip:
  | NOWAIT
  | SKIP LOCKED
  | EMPTY
-}
forLockingItem = do
  _strength <- forLockingStrength
  _rels <- optional $ do
    try $ space1 *> string' "of" *> space1
    nonEmptyList qualifiedName
  _nowaitOrSkip <- optional $ try $ do
    space1
    False <$ string' "nowait" <|> True <$ keyphrase "skip locked"
  return (ForLockingItem _strength _rels _nowaitOrSkip)

{-
for_locking_strength:
  | FOR UPDATE
  | FOR NO KEY UPDATE
  | FOR SHARE
  | FOR KEY SHARE
-}
forLockingStrength =
  UpdateForLockingStrength <$ try (string' "for update") <|>
  NoKeyUpdateForLockingStrength <$ try (string' "for no key update") <|>
  ShareForLockingStrength <$ try (string' "for share") <|>
  KeyForLockingStrength <$ try (string' "for key share")


-- * References & Names
-------------------------

quotedName :: Parser Name
quotedName = label "quoted name" $ try $ do
  _contents <- quotedString '"'
  if Text.null _contents
    then fail "Empty name"
    else return (QuotedName _contents)

ident :: Parser Name
ident = quotedName <|> keywordNameByPredicate (not . Predicate.keyword)

{-
ColId:
  |  IDENT
  |  unreserved_keyword
  |  col_name_keyword
-}
{-# NOINLINE colId #-}
colId :: Parser Name
colId = ident <|> keywordNameFromSet (HashSet.unreservedKeyword <> HashSet.colNameKeyword)

{-
ColLabel:
  |  IDENT
  |  unreserved_keyword
  |  col_name_keyword
  |  type_func_name_keyword
  |  reserved_keyword
-}
colLabel :: Parser Name
colLabel = ident <|> keywordNameFromSet HashSet.keyword

{-|
>>> testParser qualifiedName "a.b"
IndirectedQualifiedName (UnquotedName "a") (AttrNameIndirectionEl (UnquotedName "b") :| [])
-}
{-
qualified_name:
  | ColId
  | ColId indirection
-}
qualifiedName :: Parser QualifiedName
qualifiedName = do
  _a <- try colId
  asum
    [
      do
        _b <- indirection
        return (IndirectedQualifiedName _a _b)
      ,
      pure (SimpleQualifiedName _a)
    ]

{-
columnref:
  | ColId
  | ColId indirection
-}
columnRef = qualifiedName

{-
func_name:
  | type_function_name
  | ColId indirection
type_function_name:
  | IDENT
  | unreserved_keyword
  | type_func_name_keyword
-}
funcName =
  SimpleQualifiedName <$> ident <|>
  SimpleQualifiedName <$> keywordNameFromSet (HashSet.unreservedKeyword <> HashSet.typeFuncNameKeyword) <|>
  IndirectedQualifiedName <$> try colId <*> (space *> indirection)

{-
indirection:
  | indirection_el
  | indirection indirection_el
-}
indirection :: Parser Indirection
indirection = some indirectionEl

{-
indirection_el:
  | '.' attr_name
  | '.' '*'
  | '[' a_expr ']'
  | '[' opt_slice_bound ':' opt_slice_bound ']'
opt_slice_bound:
  | a_expr
  | EMPTY
-}
indirectionEl :: Parser IndirectionEl
indirectionEl =
  asum
    [
      do
        try (space *> char '.' *> space)
        AllIndirectionEl <$ char '*' <|> AttrNameIndirectionEl <$> attrName
      ,
      do
        try (space *> char '[' *> space)
        _a <-
          asum
            [
              do
                _a <- try (optional aExpr <* space <* char ':')
                space
                _b <- optional aExpr
                return (SliceIndirectionEl _a _b)
              ,
              ExprIndirectionEl <$> aExpr
            ]
        space
        char ']'
        return _a
    ]

{-
attr_name:
  | ColLabel
-}
attrName = colLabel

keywordNameFromSet :: HashSet Text -> Parser Name
keywordNameFromSet _set = keywordNameByPredicate (Predicate.inSet _set)

keywordNameByPredicate :: (Text -> Bool) -> Parser Name
keywordNameByPredicate _predicate = try $ do
  _keyword <- keyword
  if _predicate _keyword
    then return (UnquotedName _keyword)
    else fail ("Reserved keyword " <> show _keyword <> " used as an identifier. If that's what you intend, you have to wrap it in double quotes.")

keyword :: Parser Text
keyword = label "keyword" $ try $ do
  _firstChar <- satisfy Predicate.firstIdentifierChar
  _remainder <- takeWhileP Nothing Predicate.notFirstIdentifierChar
  return (Text.cons _firstChar _remainder)

{-|
Consume a keyphrase, ignoring case and types of spaces between words.
-}
keyphrase :: Text -> Parser Text
keyphrase a = Text.words a & fmap (void . string') & intersperse space1 & sequence_ & fmap (const a) & try & label (show a)
