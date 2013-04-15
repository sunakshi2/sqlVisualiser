module SQLParser where
import DataTypes
import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.Expr
import Control.Applicative
import Control.Monad (MonadPlus(..), ap)
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>), Column)

 
-- | Parser for parsing names of tables
sql_table= TableRecord <$> sql_identifier
                       <*> optionMaybe (sql_reserved "AS" *> sql_identifier <?> "an identifier" ) 

-- | Parser for parsing 'ColumnRecord' and checking valid format for a column name.                       
sql_columns :: Parser ColumnRecord
sql_columns = try sql_usingtable
            <|> sql_column
            <?> "column"
    where sql_usingtable = ColumnRecord <$> (Just <$> sql_identifier)
                            <*> (sql_dot *> sql_identifier)
          sql_column = ColumnRecord Nothing <$> sql_identifier
-- | Parser for parsing 'ColumnList' and checking the column names format and the aggregate functions as well          
sql_ColumnRecord :: Parser ColumnList
sql_ColumnRecord = ColumnList <$> (sepBy1 sql_columns sql_comma)
                              <*> (optionMaybe sql_aggregate)

-- | Parser for 'Aggregate' functions.                                              
sql_aggregate = Aggregate <$> sql_agfunctions
                          <*> sql_identifier
-- | Parser for 'Aggregate' functions.                                              
sql_agfunctions :: Parser AggregateFunctions
sql_agfunctions = Max    <$ sql_reserved "max"
              <|> Min    <$ sql_reserved "min"
              <|> Count  <$ sql_reserved "count"
              <|> Avg    <$ sql_reserved "avg"
              <|> Sum    <$ sql_reserved "sum"
              <?> "Aggregate function"              
                
-- | Parser for parsing the expressions in an SQL query statement
sql_Expr :: Parser Expr
sql_Expr = buildExpressionParser opTable sql_Expr' <?> "Expr"
                where sql_Expr' = sql_parens sql_Expr
                                        <|> Column <$> sql_columns
                                        -- <|> NestedSelect <$> sql_select
                                        <|> number <$> sql_numeric
                                        <|> StringValue <$> sql_stringValue
                                        <|> CharValue <$> sql_charValue
                                        <|> BoolValue True <$ sql_reserved "TRUE"
                                        <|> BoolValue False <$ sql_reserved "FALSE"
                                        <|> Null <$ sql_reserved "NULL"
                      number (Left x) = IntValue x
                      number (Right x) = FloatValue x
                      
-- | Parser for SQL DROP query                      
sql_drop :: Parser SQLQuery
sql_drop = Drop <$> (sql_reserved "DROP" *> sql_reserved "TABLE" *> (sepBy1 sql_table sql_comma)) <?> "drop statement"   

-- | Parser for SQL DELETE query                   
sql_delete :: Parser SQLQuery
sql_delete = Delete <$> (sql_reserved "DELETE" *> sql_reserved "FROM" *> sql_table)
		    <*> (sql_reserved "WHERE" *> sql_Expr)

-- | Parser for SQL UPDATE query		    
sql_update :: Parser SQLQuery
sql_update = Update <$> (sql_reserved "update" *> sql_table)
       			  <*> (sql_reserved "set" *> sepBy1 sql_set sql_comma)
                  <*> optionMaybe (sql_reserved "where" *> sql_Expr)
                  <?> "update statement"

-- | Parser for SET inside UPDATE query
sql_set :: Parser (ColumnRecord, Expr)
sql_set = (,) <$> sql_columns <* sql_reservedOp "="  <*> sql_Expr
             <?> "update statement"		
 
 -- | Parser for SQL CREATE query           
sql_create :: Parser SQLQuery
sql_create = Create <$> (sql_reserved "create" *> sql_reserved "table" *> sql_table)
                    <*> (sql_parens (sepBy1 sql_createcolumn sql_comma))
                    <?> "create statement"
 
 -- | Parser for creating columns with data types inside CREATE query                   
sql_createcolumn :: Parser ColumnPairs                    
sql_createcolumn = ColumnPairs <$> sql_identifier 
                               <*> sql_columntype
 
 -- | Parser for SQL column types                                     
sql_columntype :: Parser ColumnTyp                    
sql_columntype = IntTyp    <$ sql_reserved "int"
             <|> DoubleTyp <$ sql_reserved "double"
             <|> StringTyp <$ sql_reserved "string"
             <|> CharTyp   <$ sql_reserved "char"
             <|> BoolTyp   <$ sql_reserved "bool"
             <?> "column type"

-- | Parser for SQL INSERT query           
sql_insert :: Parser SQLQuery
sql_insert = Insert <$> (sql_reserved "insert" *> sql_reserved "into" *> sql_table)
                    <*> optionMaybe (sql_parens $ sepBy1 sql_columns sql_comma)
                    <*> (sql_reserved "values" *> (sql_parens $ sepBy1 sql_Expr sql_comma))
 
 -- | Parser for SQL SELECT query                  
sql_select :: Parser SQLQuery
sql_select = Select <$> (sql_reserved "select" *> sql_selectColumns)
                  <*> (sql_reserved "from" *> sepBy1 sql_table sql_comma)
                  <*> optionMaybe (sql_reserved "where" *> sql_Expr)
                  <*> optionMaybe (sql_reserved "group by" *> sql_group)
                  <*> optionMaybe (sql_reserved "order by" *> sql_order)
                  <?> "select statement"
                  where sql_selectColumns :: Parser (Either Star ColumnList)
                        sql_selectColumns = Left Everything <$ sql_reservedOp "*"
                                      <|> Right <$> sql_ColumnRecord
                                      <?> "column list"
                        sql_order = OrderBy <$> (sepBy1 sql_columns sql_comma)
                                        <*> try (string "asc" <|> string "desc")
                                        <?> "Order statement"
 
 -- | Parser for SQL GROUP BY uses 'sql_havingExpr'                    
sql_group = GroupBy <$> sql_columns
                  <*> optionMaybe (sql_reserved "having" *> sql_havingExpr)
 
 -- | Parser for SQL HAVING clause               
sql_havingExpr :: Parser Expr
sql_havingExpr =  buildExpressionParser opTable sql_havingexp' <?> "Expression"
                where sql_havingexp' = sql_parens sql_Expr
                                        <|> Column <$> sql_columns
                                        <|> SQLFunction <$> sql_aggregate
                                        <|> extractNum <$> sql_numeric
                                        <|> StringValue <$> sql_stringValue
                                        <|> CharValue <$> sql_charValue
                                        <|> BoolValue True <$ sql_reserved "true"
                                        <|> BoolValue False <$ sql_reserved "false"
                                        <|> Null <$ sql_reserved "null"
                      extractNum (Left x) = IntValue x
                      extractNum (Right x) = FloatValue x                                                       
 
 -- | Parser that calls parser for different SQL querries.                     
sql_sqlStatement :: Parser SQLQuery
sql_sqlStatement =    sql_drop
		          <|> sql_delete
		          <|> sql_update
		          <|> sql_create
		          <|> sql_insert
		          <|> sql_select


-- | Parses SQL statements                  
sql_query = parse (sql_whiteSpace *> many (sql_sqlStatement <* sql_semi) <* eof) "Error while parsing SQL statement"           
                      
