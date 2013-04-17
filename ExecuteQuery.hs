{- |
Module      :  <File name or $Header$ to be replaced automatically>
Description :  Takes SQL query, parses it and writes the execution sequence to a file
Copyright   :  (c) <Akanksha , Deepali, Sonam, Sunakshi>
-}
module ExecuteQuery where
import SQLParser
import ShowGraphics
import Data.List
import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.Expr
import Control.Applicative
import Control.Monad (MonadPlus(..), ap)
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>), Column)

-- for aggregate
-- | To remove parenthesis in aggregate function.
construct [] = []
construct (')':xs) = construct xs
construct (x:xs) = if (x =='(') then (' ':(construct xs))
                    else (x:(construct xs))           

-- | Checks if query contains aggregate function and calls 'sql_query'.
change str = if (("max" `elem` (words aggregatefunc))||("min" `elem` (words aggregatefunc))||("avg" `elem` (words aggregatefunc))||("count" `elem` (words aggregatefunc))||("sum" `elem` (words aggregatefunc))||("group" `elem` (words aggregatefunc)))
		then (sql_query aggregatefunc)
		else (sql_query str)
	     where aggregatefunc = (unwords (change1 (words $ construct str)))

-- | Separating aggregate function.
change1 (y : z :[]) = [y,z]
change1 (y : z : ys) = if ((z == "max")||(z == "min")||(z == "sum")||(z == "avg")||(z == "count"))
                         then if (y == ",") then (" ":z:ys)
                         	else (y:z:ys)
                         else (y :change1( z : ys))
                         


-- FOR WRITING IN A FILE      
-- | Functions that takes query; calls 'sql_query' or 'change' and writes the execution order by calling 'querytype' in the file.
 
fileop :: String -> IO()
fileop sqlstring = if (("any" `elem` (words sqlstring))||("all" `elem` (words sqlstring))||("exists" `elem` (words sqlstring)))
		    then case (sql_query sqlstring) of
                        	Left x -> writeError x
                        	Right y -> querytype sqlstring
	            else case (change sqlstring) of
                        	Left x -> writeError x
                        	Right y -> querytype sqlstring            	
                        
-- | Gets type of query.
querytype query = case (fst (break (== ' ') query)) of
                        "select" -> selectquery query 

-- | Writes parse Error to file.          
writeError :: ParseError -> IO()
writeError a = writeFile "writefile.txt" (show a)  

-- | Checks if SELECT query has any,all,exists and calls 'anyall' ,'existquery', 'groupquery' and 'selectsql' to write execution order to file.
selectquery str = if (("any" `elem` (words str)) || ("all" `elem` (words str)))
                	then (anyall str)
                    	else 
                        if ("exists" `elem` (words str))
                            then (existquery str)
                            else if (("max" `elem` (words str))||("min" `elem` (words str))||("avg" `elem` (words str))||("count" `elem` (words str))||("sum" `elem` (words str))||("group" `elem` (words str)))
                            	   then (groupquery str)
                            else (selectsql (str \\ [';']))
 
-- | Helping function for 'anyall'                   
anyallnest str = fun xs (words (str \\ ['(',')',';']))
                     where xs = ["from","where","select","from","where"]

-- | Execution order for nested SELECT querries with any, all calls 'anyallnest' and 'arrange'.  
anyall str = writeFile "writefile.txt" (unwords (intersperse "\n" ((arrange (snd result))++["NS"]++(arrange (fst result))++[unwords (snd orderby)])))
                where result = splitAt 3 (anyallnest (unwords (fst orderby)))
                      orderby = break (== "order") (words str)

-- | Execution order for nested SELECT querries with EXISTS or NOT EXISTS.
existquery str = writeFile "writefile.txt" (unwords (intersperse "\n" ((arrange (fst result))++["NS"]++(arrange (snd result)++[unwords (snd orderby)]))))
                where result = splitAt 3 (anyallnest (unwords (fst orderby)))
                      orderby = break (== "order") (words str)

-- | Helping function for getting Execution order.
arrange str = [(head (tail str))] ++ [(last str)] ++ [(head str)]     

-- | Execution order for SELECT query calls 'fun'
selectsql :: String -> IO()
selectsql a = let z = fun xs (fst orderby) 
                in writeFile "writefile.txt" ((head(tail z))++"\n"++(last z)++"\n"++(head z)++"\n"++(unwords (snd orderby)))
                where xs = ["from","where"]  
                      orderby = break (== "order") (words a)     
 
-- | Helper function  for 'selectsql'             
fun [] y = [unwords y]
fun y [] = []
fun xs ys = [unwords (fst a)] ++ (fun (tail xs) (snd a))
            where a = break (== (head xs)) ys   
 
-- Syntax for orderby 
-- | Functions to check the constraint : columns inside ORDER BY is part of columns inside SELECT.
removecomma str = if (',' `elem` str)
                    then removecomma (str \\ [','])
                    else str

-- | Checks constraint : columns in ORDER BY is part of columns inside SELECT.
orderelement [] y = True
orderelement y [] = False
orderelement _ ["*"] = True
orderelement (x:xs) y = if x `elem` y
                        then orderelement xs y
                        else False
 
-- | Finds list of columns inside SELECT.          
columnlist str = takeWhile (/="from") (tail (words (removecomma str)))

-- | Finds list of columns inside ORDER BY.
orderlist str = takeWhile (/="by") (tail (reverse (words (removecomma str))))

-- | Checks ORDER BY constraint by calling 'orderelement' and writes execution order to file.
ordersyntax str = if (orderelement ord col)                    
                    then (fileop str)
                    else writeFile "writefile.txt" "Error columns order not correct"
                  where col = takeWhile (/="from") (tail (words (removecomma str)))
                        ord = takeWhile (/="by") (tail (reverse (words (removecomma str))))
                        
-- printing groupby
-- | Breaks GROUP BY querries.
breakgroup str = if("where" `elem` (words str))
		   then fun xs (words (str \\ [';']))
		   else fun xs1 (words (str \\ [';']))
		where xs = ["from","where","group","having","order"]
		      xs1 = ["from","group","having","order"]

-- | Writes execution order for GROUP BY query.
groupquery str = if("where" `elem` (words str))
		   then writeFile "writefile.txt" ((head (tail x))++"\n"++(head (tail (tail x)))++"\n"++(head (tail (tail (tail x))))++"\n"++(head (tail (tail (tail (tail x)))))++"\n"++(head x)++"\n"++(last x))
		   else writeFile "writefile.txt" ((head (tail x))++"\n"++(head (tail (tail x)))++"\n"++(head (tail (tail (tail x))))++"\n"++(head x)++"\n"++(last x))

		where x = (breakgroup str)           
		
-- syntax checking of insert           
-- | Function to remove parenthesis from aggregate function.
removebracket str = if (("(" `elem` str) || (")" `elem` str))
                    then removebracket (str \\ ["(",")"])
                    else str

-- | Checks if values in INSERT query is same as number of columns; using 'removebracket'.         
insertsyntax str = if ((length col) == (length value))
                    then writeFile "writefile.txt" (show (sql_query str))
                    else writeFile "writefile.txt" "\"Error no of columns does not match with no of values"
                    where col = removebracket (dropWhile (/="(") (takeWhile (/=")") (words (removecomma str))))                    
                          value = removebracket (dropWhile (/="(") (dropWhile (/=")") (words (removecomma str))))  	
	             
-- | Checks for SELECT or INSERT query and accordingly calls 'fileop'        
callSqlParser str = if ("select" `elem` (words str))
                then if ("order" `elem` (words str))
                       then (ordersyntax str)
                       	else (fileop str)
                else if ("insert" `elem` (words str))
                	then (insertsyntax str)
                	else writeFile "writefile.txt" (show (sql_query str))
  
-- | Main function which instanciate function like 'callSqlParser' and 'display'                        
<<<<<<< HEAD
main query = do
                callSqlParser query
                display                             
=======
sqlQuery  query = do
                                        callSqlParser query
                                        display                             
>>>>>>> 38dfdf0e16a59805f0e78f9d5374fdbbab858148
                                                                
