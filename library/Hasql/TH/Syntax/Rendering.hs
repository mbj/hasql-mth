module Hasql.TH.Syntax.Rendering where

import Hasql.TH.Prelude hiding (expr, try, option, many, sortBy)
import Hasql.TH.Syntax.Ast
import Data.ByteString.FastBuilder
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Builder.Scientific as Scientific
import qualified Data.ByteString.Builder as BsBuilder
import qualified Data.ByteString.Lazy as LazyBs


-- * Execution
-------------------------

toByteString :: Builder -> ByteString
toByteString = toStrictByteString

toText :: Builder -> Text
toText = Text.decodeUtf8 . toByteString


-- * Helpers
-------------------------

scientific :: Scientific -> Builder
scientific a = Scientific.scientificBuilder a & BsBuilder.toLazyByteString & LazyBs.toStrict & byteString

text :: Text -> Builder
text = stringUtf8 . Text.unpack

nonEmptyList :: (a -> Builder) -> NonEmpty a -> Builder
nonEmptyList = intersperseFoldMap1 ", "

lexemes :: [Builder] -> Builder
lexemes = mconcat . intersperse " "

optLexemes :: [Maybe Builder] -> Builder
optLexemes = lexemes . catMaybes

inParens :: Builder -> Builder
inParens a = "(" <> a <> ")"


-- * Select
-------------------------

preparableStmt :: PreparableStmt -> Builder
preparableStmt = \ case
  SelectPreparableStmt a -> selectStmt a

selectStmt :: SelectStmt -> Builder
selectStmt = \ case
  InParensSelectStmt a -> inParens (selectStmt a)
  NoParensSelectStmt a -> selectNoParens a

selectNoParens :: SelectNoParens -> Builder
selectNoParens (SelectNoParens a b c d e) =
  optLexemes
    [
      fmap withClause a,
      Just (selectClause b),
      fmap sortClause c,
      fmap selectLimit d,
      fmap forLockingClause e
    ]

withClause (WithClause a b) =
  "WITH " <> bool "" "RECURSIVE " a <> nonEmptyList commonTableExpr b

commonTableExpr (CommonTableExpr a b c d) =
  optLexemes
    [
      Just (name a),
      fmap (nonEmptyList name) b,
      Just "AS",
      fmap materialization c,
      Just (inParens (preparableStmt d))
    ]

materialization = bool "NOT MATERIALIZED" "MATERIALIZED"

selectLimit = \ case
  LimitOffsetSelectLimit a b -> lexemes [limitClause a, offsetClause b]
  OffsetLimitSelectLimit a b -> lexemes [offsetClause a, limitClause b]
  LimitSelectLimit a -> limitClause a
  OffsetSelectLimit a -> offsetClause a

limitClause = \ case
  LimitLimitClause a b -> optLexemes [Just (selectLimitValue a), fmap expr b]
  FetchOnlyLimitClause a b c ->
    optLexemes
      [
        Just "FETCH",
        Just (firstOrNext a),
        fmap selectFetchFirstValue b,
        Just (rowOrRows c),
        Just "ONLY"
      ]

firstOrNext = bool "FIRST" "NEXT"

rowOrRows = bool "ROW" "ROWS"

selectFetchFirstValue = \ case
  ExprSelectFetchFirstValue a -> expr a
  NumSelectFetchFirstValue a b -> bool "+" "-" a <> intOrFloat b

intOrFloat = either integerDec scientific

selectLimitValue = \ case
  ExprSelectLimitValue a -> expr a
  AllSelectLimitValue -> "ALL"

offsetClause = \ case
  ExprOffsetClause a -> "OFFSET " <> expr a
  FetchFirstOffsetClause a b -> "OFFSET " <> selectFetchFirstValue a <> " " <> rowOrRows b

forLockingClause = \ case
  ItemsForLockingClause a -> nonEmptyList forLockingItem a
  ReadOnlyForLockingClause -> "FOR READ ONLY"

forLockingItem (ForLockingItem a b c) =
  optLexemes
    [
      Just (forLockingStrength a),
      fmap lockedRelsList b,
      fmap nowaitOrSkip c
    ]

forLockingStrength = \ case
  UpdateForLockingStrength -> "FOR UPDATE"
  NoKeyUpdateForLockingStrength -> "FOR NO KEY UPDATE"
  ShareForLockingStrength -> "FOR SHARE"
  KeyForLockingStrength -> "FOR KEY SHARE"

lockedRelsList a = "OF " <> nonEmptyList qualifiedName a

nowaitOrSkip = bool "NOWAIT" "SKIP LOCKED"

selectClause :: SelectClause -> Builder
selectClause = either simpleSelect selectNoParens

simpleSelect :: SimpleSelect -> Builder
simpleSelect = \ case
  NormalSimpleSelect a b c d e f g ->
    optLexemes
      [
        Just "SELECT",
        fmap targeting a,
        fmap intoClause b,
        fmap fromClause c,
        fmap whereClause d,
        fmap groupClause e,
        fmap havingClause f,
        fmap windowClause g
      ]
  ValuesSimpleSelect a -> valuesClause a
  BinSimpleSelect _ a _ b -> selectClause a <> selectClause b

targeting :: Targeting -> Builder
targeting = \ case
  NormalTargeting a -> nonEmptyList target a
  AllTargeting a -> "ALL" <> foldMap (mappend " " . nonEmptyList target) a
  DistinctTargeting a b -> "DISTINCT" <> foldMap (mappend " " . onExpressionsClause) a <> " " <> nonEmptyList target b

onExpressionsClause :: NonEmpty Expr -> Builder
onExpressionsClause a = "ON (" <> nonEmptyList expr a <> ")"

target :: Target -> Builder
target = \ case
  AllTarget -> "*"
  ExprTarget a b -> expr a <> foldMap (mappend " " . name) b


-- * Select Into
-------------------------

intoClause :: IntoClause -> Builder
intoClause a = "INTO " <> optTempTableName a

optTempTableName :: OptTempTableName -> Builder
optTempTableName (OptTempTableName a b c) =
  optLexemes
    [
      if a then Just "TEMP" else Nothing,
      if b then Just "UNLOGGED" else Nothing,
      Just (qualifiedName c)
    ]


-- * From
-------------------------

fromClause :: FromClause -> Builder
fromClause a = "FROM " <> nonEmptyList tableRef a

tableRef :: TableRef -> Builder
tableRef = \ case
  RelationExprTableRef a b ->
    optLexemes
      [
        Just (relationExpr a),
        fmap aliasClause b
      ]
  SelectTableRef a b c ->
    optLexemes
      [
        if a then Just "LATERAL" else Nothing,
        Just (selectNoParens b),
        fmap aliasClause c
      ]
  JoinTableRef a b -> case b of
    Just c -> inParens (joinedTable a) <> " " <> aliasClause c
    Nothing -> joinedTable a

relationExpr :: RelationExpr -> Builder
relationExpr = \ case
  SimpleRelationExpr a b -> qualifiedName a <> bool "" " *" b
  OnlyRelationExpr a b -> "ONLY " <> bool qualifiedName (inParens . qualifiedName) b a

aliasClause :: AliasClause -> Builder
aliasClause (AliasClause a b) =
  optLexemes
    [
      Just "AS",
      Just (name a),
      fmap (inParens . nonEmptyList name) b
    ]

joinedTable :: JoinedTable -> Builder
joinedTable = \ case
  InParensJoinedTable a -> inParens (joinedTable a)
  MethJoinedTable a b c -> case a of
    CrossJoinMeth -> tableRef b <> " CROSS JOIN " <> tableRef c
    QualJoinMeth d e -> tableRef b <> foldMap (mappend " " . joinType) d <> " JOIN " <> tableRef c <> " " <> joinQual e
    NaturalJoinMeth d -> tableRef b <> " NATURAL" <> foldMap (mappend " " . joinType) d <> " " <> tableRef c

joinType :: JoinType -> Builder
joinType = \ case
  FullJoinType a -> "FULL" <> if a then " OUTER" else ""
  LeftJoinType a -> "LEFT" <> if a then " OUTER" else ""
  RightJoinType a -> "RIGHT" <> if a then " OUTER" else ""
  InnerJoinType -> "INNER"

joinQual :: JoinQual -> Builder
joinQual = \ case
  UsingJoinQual a -> "USING (" <> nonEmptyList name a <> ")" 
  OnJoinQual a -> "ON " <> expr a


-- * Where
-------------------------

whereClause :: Expr -> Builder
whereClause a = "WHERE " <> expr a


-- * Group By
-------------------------

groupClause :: GroupClause -> Builder
groupClause a = "GROUP BY " <> nonEmptyList groupByItem a

groupByItem :: GroupByItem -> Builder
groupByItem = \ case
  ExprGroupByItem a -> expr a
  EmptyGroupingSetGroupByItem -> "()"
  RollupGroupByItem a -> "ROLLUP (" <> nonEmptyList expr a <> ")"
  CubeGroupByItem a -> "CUBE (" <> nonEmptyList expr a <> ")"
  GroupingSetsGroupByItem a -> "GROUPING SETS (" <> nonEmptyList groupByItem a <> ")"


-- * Having
-------------------------

havingClause :: Expr -> Builder
havingClause a = "HAVING " <> expr a


-- * Window
-------------------------

windowClause :: NonEmpty WindowDefinition -> Builder
windowClause a = "WINDOW " <> nonEmptyList windowDefinition a

windowDefinition :: WindowDefinition -> Builder
windowDefinition (WindowDefinition a b) = name a <> " AS " <> windowSpecification b

windowSpecification :: WindowSpecification -> Builder
windowSpecification (WindowSpecification a b c d) =
  inParens $ optLexemes
    [
      fmap name a,
      fmap partitionClause b,
      fmap sortClause c,
      fmap frameClause d
    ]

partitionClause :: NonEmpty Expr -> Builder
partitionClause a = "PARTITION BY " <> nonEmptyList expr a

frameClause :: FrameClause -> Builder
frameClause (FrameClause a b c) =
  optLexemes
    [
      Just (frameClauseMode a),
      Just (frameExtent b),
      fmap windowExclusionCause c
    ]

frameClauseMode :: FrameClauseMode -> Builder
frameClauseMode = \ case
  RangeFrameClauseMode -> "RANGE"
  RowsFrameClauseMode -> "ROWS"
  GroupsFrameClauseMode -> "GROUPS"

frameExtent :: FrameExtent -> Builder
frameExtent = \ case
  SingularFrameExtent a -> frameBound a
  BetweenFrameExtent a b -> "BETWEEN " <> frameBound a <> " AND " <> frameBound b

frameBound :: FrameBound -> Builder
frameBound = \ case
  UnboundedPrecedingFrameBound -> "UNBOUNDED PRECEDING"
  UnboundedFollowingFrameBound -> "UNBOUNDED FOLLOWING"
  CurrentRowFrameBound -> "CURRENT ROW"
  PrecedingFrameBound a -> expr a <> " PRECEDING"
  FollowingFrameBound a -> expr a <> " FOLLOWING"

windowExclusionCause :: WindowExclusionClause -> Builder
windowExclusionCause = \ case
  CurrentRowWindowExclusionClause -> "EXCLUDE CURRENT ROW"
  GroupWindowExclusionClause -> "EXCLUDE GROUP"
  TiesWindowExclusionClause -> "EXCLUDE TIES"
  NoOthersWindowExclusionClause -> "EXCLUDE NO OTHERS"


-- * Order By
-------------------------

sortClause :: NonEmpty SortBy -> Builder
sortClause a = "ORDER BY " <> nonEmptyList sortBy a

sortBy :: SortBy -> Builder
sortBy (SortBy a b) = optLexemes [Just (expr a), fmap order b]

order :: Order -> Builder
order = \ case
  AscOrder -> "ASC"
  DescOrder -> "DESC"


-- * Values
-------------------------

valuesClause :: ValuesClause -> Builder
valuesClause a = "VALUES " <> nonEmptyList (inParens . nonEmptyList expr) a


-- * Expr
-------------------------

expr :: Expr -> Builder
expr = \ case
  PlaceholderExpr a -> "$" <> intDec a
  TypecastExpr a b -> expr a <> " :: " <> type_ b
  BinOpExpr a b c -> expr b <> " " <> text a <> " " <> expr c
  EscapableBinOpExpr a b c d e -> optLexemes [
      Just (expr c),
      if a then Just "NOT" else Nothing,
      Just (text b),
      Just (expr d),
      fmap (mappend "ESCAPE " . expr) e
    ]
  DefaultExpr -> "DEFAULT"
  QualifiedNameExpr a -> qualifiedName a
  LiteralExpr a -> literal a
  InParensExpr a b -> "(" <> expr a <> ")" <> foldMap (mappend " " . indirection) b
  CaseExpr a b c -> optLexemes [
      Just "CASE",
      fmap expr a,
      Just (nonEmptyList whenClause b),
      fmap caseDefault c,
      Just "END"
    ]
  FuncExpr a -> funcApplication a
  SelectExpr a -> inParens (selectNoParens a)
  ExistsSelectExpr a -> "EXISTS " <> inParens (selectNoParens a)
  ArraySelectExpr a -> "ARRAY " <> inParens (selectNoParens a)
  GroupingExpr a -> "GROUPING " <> inParens (nonEmptyList expr a)

type_ :: Type -> Builder
type_ (Type a _ b _) =
  text a <>
  fold (replicate b "[]")

whenClause :: WhenClause -> Builder
whenClause (WhenClause a b) = "WHEN " <> expr a <> " THEN " <> expr b

caseDefault :: Expr -> Builder
caseDefault a = "ELSE " <> expr a

funcApplication :: FuncApplication -> Builder
funcApplication (FuncApplication a b) =
  qualifiedName a <> "(" <> foldMap funcApplicationParams b <> ")"

funcApplicationParams :: FuncApplicationParams -> Builder
funcApplicationParams = \ case
  NormalFuncApplicationParams a b c ->
    optLexemes
      [
        fmap allOrDistinct a,
        Just (nonEmptyList funcArgExpr b),
        fmap sortClause c
      ]
  VariadicFuncApplicationParams a b c ->
    optLexemes
      [
        fmap (nonEmptyList funcArgExpr) a,
        Just "VARIADIC",
        Just (funcArgExpr b),
        fmap sortClause c
      ]
  StarFuncApplicationParams -> "*"

allOrDistinct :: AllOrDistinct -> Builder
allOrDistinct = \ case
  AllAllOrDistinct -> "ALL"
  DistinctAllOrDistinct -> "DISTINCT"

funcArgExpr :: FuncArgExpr -> Builder
funcArgExpr = \ case
  ExprFuncArgExpr a -> expr a
  ColonEqualsFuncArgExpr a b -> name a <> " := " <> expr b
  EqualsGreaterFuncArgExpr a b -> name a <> " => " <> expr b


-- * Literals
-------------------------

literal :: Literal -> Builder
literal = \ case
  IntLiteral a -> integerDec a
  FloatLiteral a -> scientific a
  StringLiteral a -> stringLiteral a
  BitLiteral a -> "B'" <> text a <> "'"
  HexLiteral a -> "X'" <> text a <> "'"
  FuncLiteral a b c -> qualifiedName a <> foldMap (inParens . funcLiteralArgList) b <> " " <> stringLiteral c
  ConstTypenameLiteral a b -> constTypename a <> " " <> stringLiteral b
  StringIntervalLiteral a b -> "INTERVAL " <> stringLiteral a <> foldMap (mappend " " . interval) b
  IntIntervalLiteral a b -> "INTERVAL " <> inParens (integerDec a) <> " " <> stringLiteral b
  BoolLiteral a -> if a then "TRUE" else "FALSE"
  NullLiteral -> "NULL"

stringLiteral :: Text -> Builder
stringLiteral a = "'" <> text (Text.replace "'" "''" a) <> "'"

funcLiteralArgList (FuncLiteralArgList a b) = nonEmptyList funcArgExpr a <> foldMap (mappend " " . sortClause) b

constTypename = \ case
  NumericConstTypename a -> numeric a
  ConstBitConstTypename a -> constBit a
  ConstCharacterConstTypename a -> constCharacter a
  ConstDatetimeConstTypename a -> constDatetime a

numeric = \ case
  IntNumeric -> "INT"
  IntegerNumeric -> "INTEGER"
  SmallintNumeric -> "SMALLINT"
  BigintNumeric -> "BIGINT"
  RealNumeric -> "REAL"
  FloatNumeric a -> "FLOAT" <> foldMap (mappend " " . inParens . integerDec) a
  DoublePrecisionNumeric -> "DOUBLE PRECISION"
  DecimalNumeric a -> "DECIMAL" <> foldMap (mappend " " . inParens . nonEmptyList expr) a
  DecNumeric a -> "DEC" <> foldMap (mappend " " . inParens . nonEmptyList expr) a
  NumericNumeric a -> "NUMERIC" <> foldMap (mappend " " . inParens . nonEmptyList expr) a
  BooleanNumeric -> "BOOLEAN"

constBit (ConstBit a b) = optLexemes [
    Just "BIT",
    bool Nothing (Just "VARYING") a,
    fmap (inParens . nonEmptyList expr) b
  ]

constCharacter (ConstCharacter a b) = character a <> foldMap (mappend " " . inParens . integerDec) b

character = \ case
  CharacterCharacter a -> "CHARACTER" <> bool "" " VARYING" a
  CharCharacter a -> "CHAR" <> bool "" " VARYING" a
  VarcharCharacter -> "VARCHAR"
  NationalCharacterCharacter a -> "NATIONAL CHARACTER" <> bool "" " VARYING" a
  NationalCharCharacter a -> "NATIONAL CHAR" <> bool "" " VARYING" a
  NcharCharacter a -> "NCHAR" <> bool "" " VARYING" a

constDatetime = \ case
  TimestampConstDatetime a b -> optLexemes [
      Just "TIMESTAMP",
      fmap (inParens . integerDec) a,
      fmap timezone b
    ]
  TimeConstDatetime a b -> optLexemes [
      Just "TIME",
      fmap (inParens . integerDec) a,
      fmap timezone b
    ]

timezone = \ case
  False -> "WITH TIME ZONE"
  True -> "WITHOUT TIME ZONE"

interval :: Interval -> Builder
interval = \ case
  YearInterval -> "YEAR"
  MonthInterval -> "MONTH"
  DayInterval -> "DAY"
  HourInterval -> "HOUR"
  MinuteInterval -> "MINUTE"
  SecondInterval a -> intervalSecond a
  YearToMonthInterval -> "YEAR TO MONTH"
  DayToHourInterval -> "DAY TO HOUR"
  DayToMinuteInterval -> "DAY TO MINUTE"
  DayToSecondInterval a -> "DAY TO " <> intervalSecond a
  HourToMinuteInterval -> "HOUR TO MINUTE"
  HourToSecondInterval a -> "HOUR TO " <> intervalSecond a
  MinuteToSecondInterval a -> "MINUTE TO " <> intervalSecond a

intervalSecond :: IntervalSecond -> Builder
intervalSecond = \ case
  Nothing -> "SECOND" 
  Just a -> "SECOND " <> inParens (integerDec a)


-- * Names and refs
-------------------------

name :: Name -> Builder
name = \ case
  QuotedName a -> char7 '"' <> text (Text.replace "\"" "\"\"" a) <> char7 '"'
  UnquotedName a -> text a

qualifiedName :: QualifiedName -> Builder
qualifiedName = \ case
  SimpleQualifiedName a -> name a
  IndirectedQualifiedName a b -> name a <> indirection b

indirection :: Indirection -> Builder
indirection = foldMap indirectionEl

indirectionEl :: IndirectionEl -> Builder
indirectionEl = \ case
  AttrNameIndirectionEl a -> "." <> name a
  AllIndirectionEl -> ".*"
  ExprIndirectionEl a -> "[" <> expr a <> "]"
  SliceIndirectionEl a b -> "[" <> foldMap expr a <> ":" <> foldMap expr b <> "]"
