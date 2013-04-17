module DataTypes where

import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.Expr
import Control.Applicative
import Control.Monad (MonadPlus(..), ap)
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>), Column)

-- | Data type for expression in a SQL query representing all types of valid expressions 
data Expr = IntValue Integer
                | FloatValue Double
                | StringValue String
                | CharValue Char
                | BoolValue Bool
                | Null
                | SQLFunction Aggregate
                | NestedSelect SQLQuery
                | Column ColumnRecord  
                | Exists Expr
                | NotExists Expr
                | GtAny Expr Expr
                | GteAny Expr Expr
                | LteAny Expr Expr
                | LtAny Expr Expr
                | EqAny Expr Expr
                | GtAll Expr Expr
                | GteAll Expr Expr
                | LteAll Expr Expr
                | LtAll Expr Expr
                | EqAll Expr Expr
                | NotEq Expr Expr 
                | NotEqAny Expr Expr 
                | NotEqAll Expr Expr 
                | Pow Expr Expr   
                | Mul Expr Expr  
                | Div Expr Expr   
                | Mod Expr Expr   
                | Plus Expr Expr  
                | Minus Expr Expr 
                | Lt Expr Expr    
                | Gt Expr Expr    
                | Lte Expr Expr   
                | Gte Expr Expr  
                | Eq Expr Expr    
                | Is Expr Expr    
                | And Expr Expr  
                | Or Expr Expr    
                | Pos Expr              
                | Neg Expr              
                | Not Expr              
                deriving Show
                
-- | Data type for SQL aggregate function  
data Aggregate = Aggregate { aggregatefunction :: AggregateFunctions
                           , aggregatecolumn :: String
                             } deriving Show   
                             
-- | Data type for a Column record
data ColumnRecord= ColumnRecord{
                                colId :: Maybe String
                               ,colName :: String
                               } deriving Show
-- | Data type for a list of columns. An SQL query may contain column names as well as aggregate functions in select statement so it contains a record having a list of 'ColumnRecord' and 'Aggregate'                               
data ColumnList = ColumnList { columnlist :: [ColumnRecord]
                             , aggregate :: Maybe Aggregate
                               } deriving Show   
-- | Data type for Groupby clause. A Groupby has some column names ('ColumnRecord') and an optional having 'Expr'                                
data Group = GroupBy { groupclause :: ColumnRecord
                     , having      :: Maybe Expr
                     }deriving Show   
-- | Data type for Orderby clause. An Orderby is followed by the list of column names('ColumnRecord') and the order('asc' or'desc')                     
data Order = OrderBy { orderByColumn :: [ColumnRecord]
                     , order         :: String
                     }deriving Show  
-- | Data type for aggregate functions ( Min, Max, Count, Avg, Sum)                     
data AggregateFunctions = Max
                        | Min
                        | Count
                        | Avg
                        | Sum 
                   deriving Show                                                                    
                                                             
-- | Data type for an SQL query ( Select, Insert, Delete, Drop, Create, Update)
data SQLQuery = Select {     select            :: Either Star ColumnList
                           , from              :: [TableRecord]
                           , wherecondition    :: Maybe Expr
                           , groupby           :: Maybe Group
                           , orderby           :: Maybe Order
                           } 
               | Insert { insTable :: TableRecord
                         ,insColumns :: Maybe [ColumnRecord]
                         ,values :: [Expr]
                        }
               | Delete { delTable :: TableRecord
                         ,delWherecondition :: Expr 
                         } 
               | Drop { dropTable:: [TableRecord]
                      }
               | Create { createTable :: TableRecord
                         ,createColumns :: [ColumnPairs]
                         }     
               | Update { updTable:: TableRecord
                         ,set :: [(ColumnRecord,Expr)]   
                         ,updWherecondition :: Maybe Expr
                         }
                 deriving Show  
-- | Data type for selecting everything from table as * in query "select * from tablename;"                 
data Star= Everything deriving Show 
-- | Pairs of column name and the columntype ('ColumnTyp') as given in create table command " create table tabname (colm1 type1,col2 type2);"         
data ColumnPairs= ColumnPairs String ColumnTyp deriving Show
-- | Valid types for a column in a table (Int,Double,String,Char,Bool)
data ColumnTyp= IntTyp
                | DoubleTyp
                | StringTyp
                | CharTyp
                | BoolTyp
		deriving Show
-- | Data type for a Table record		
data TableRecord = TableRecord {
				   tabName :: String
				 , tabId  :: Maybe String
				} deriving Show 
				
				      
				
				
-- | Defining all types of tokens that can appear in an input query
sqlDef :: LanguageDef st
sqlDef = emptyDef{ commentStart = "/*"
                   , commentEnd = "*/"
                   , identStart = letter
                   , identLetter = alphaNum <|> char '_'
                   , reservedOpNames = [ ".", "+", "-", "^", "*" , "/", "%", "<", ">", "="]
                   , reservedNames = ["select", "from", "where", "as", "order by", "group by", "not exists" -- select statement
                                     , "is" , "not", "and", "or"       -- operators
                                     , "true", "false", "null"         -- literals
                                     , "create", "table"               -- create statement
                                     , "int", "double", "string"       -- data types
                                     , "char" , "bool"
                                     , "update", "set"                 -- update statement
                                     , "insert" , "into", "values"     -- insert statement
                                     , "drop", "table", "if", "exists" -- drop statement
                                     , "max", "min", "count", "avg", "sum" --aggregate functions
                                     , "> any", "< any", "<= any", ">= any", "=  any"
                                     , "> all", "< all", "<= all", ">= all", "=  all"
                                     , "<>", "<> any", "<> all"
                                     ]
                   , caseSensitive = False
                   }
-- | This is a record containing various parsers for the tokens defined in 'sqlDef'                   
TokenParser{ identifier  = sql_identifier
           , reserved       = sql_reserved
           , reservedOp     = sql_reservedOp
           , parens         = sql_parens
           , naturalOrFloat = sql_numeric
           , stringLiteral  = sql_stringValue
           , charLiteral    = sql_charValue
           , dot            = sql_dot
           , comma          = sql_comma
           , whiteSpace     = sql_whiteSpace
           , semi           = sql_semi
           } = makeTokenParser sqlDef
           
           
-- | This is Operator Precedance Table defined for all the operators used in SQL (as defined in sqlDef)           
opTable = [ [ Prefix (Neg   <$ sql_reservedOp    "-")
            , Prefix (Pos   <$ sql_reservedOp    "+") ]
          , [ Infix  (Pow   <$ sql_reservedOp    "^") AssocLeft ]
          , [ Infix  (Mul   <$ sql_reservedOp    "*") AssocLeft
            , Infix  (Div   <$ sql_reservedOp    "/") AssocLeft
            , Infix  (Mod   <$ sql_reservedOp    "%") AssocLeft ]
          , [ Infix  (Plus  <$ sql_reservedOp    "+") AssocLeft
            , Infix  (Minus <$ sql_reservedOp    "-") AssocLeft ]
          , [ Infix  (LtAny    <$ sql_reservedOp    "< any") AssocLeft 
            , Infix  (GtAny    <$ sql_reserved    "> any") AssocLeft
            , Infix  (LteAny    <$ sql_reserved    "<= any") AssocLeft 
            , Infix  (GteAny    <$ sql_reserved    ">= any") AssocLeft ]
          , [ Infix  (LtAll    <$ sql_reserved    "< all") AssocLeft 
            , Infix  (GtAll    <$ sql_reserved    "> all") AssocLeft
            , Infix  (LteAll    <$ sql_reserved    "<= all") AssocLeft 
            , Infix  (GteAll    <$ sql_reserved    ">= all") AssocLeft ]
          , [ Infix  (EqAny    <$ sql_reserved    "= any") AssocRight ]
          , [ Infix  (EqAll    <$ sql_reserved    "= all") AssocRight ]
          
          , [ Infix  (NotEqAll    <$ sql_reserved    "<> all") AssocLeft ]
          , [ Infix  (NotEqAny    <$ sql_reserved    "<> any") AssocLeft ]
          , [ Infix  (NotEq    <$ sql_reserved    "<>") AssocRight ]
          , [ Infix  (Is    <$ sql_reserved     "is") AssocLeft ]
          , [ Infix  (Lt    <$ sql_reservedOp    "<") AssocLeft
            , Infix  (Gt    <$ sql_reservedOp    ">") AssocLeft
            , Infix  (Lte   <$ sql_reservedOp   "<=") AssocLeft
            , Infix  (Gte   <$ sql_reservedOp   ">=") AssocLeft ]
          , [ Infix  (Eq    <$ sql_reservedOp    "=") AssocRight ]
          , [ Prefix (Not   <$ sql_reserved    "not") ]
          , [ Infix  (And   <$ sql_reserved    "and") AssocLeft ]
          , [ Infix  (Or    <$ sql_reserved     "or") AssocLeft ]
          , [ Prefix (Exists   <$ sql_reserved    "exists") ]
          , [ Prefix (NotExists   <$ sql_reserved    "not exists") ]
          
        ] 			                    
