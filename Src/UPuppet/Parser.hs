{------------------------------------------------------------------------------
    uPuppet: Parser
------------------------------------------------------------------------------}

-- ParSec docs here:
-- http://research.microsoft.com/en-us/um/people/daan/download/parsec/parsec.pdf

module UPuppet.Parser ( parsePuppet ) where

import UPuppet.Errors
import UPuppet.Options
import UPuppet.CState
import UPuppet.AST

import Control.Monad(guard)
import Data.Char(toLower)
import Data.List(intercalate,find)
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Language
import Text.ParserCombinators.Parsec.Pos
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P

{------------------------------------------------------------------------------
    lexer
------------------------------------------------------------------------------}

type PuppetLanguageDef st = GenLanguageDef String st IO

puppetDef :: PuppetLanguageDef st
puppetDef = P.LanguageDef
	{ P.commentStart   	= "/*"
	, P.commentEnd     	= "*/"
	, P.commentLine    	= "#"
	, P.nestedComments 	= True
	, P.caseSensitive 	= True
	, P.identStart = letter
	, P.identLetter = alphaNum
	, P.opStart = oneOf "&=!|<>+*/-?,:oa"
	, P.opLetter = oneOf "&=!|<>+*/-?,:"
	, P.reservedOpNames = ["+","-","/","*", "==", "!=", ">", "<", ">=", "<=", "?", "=>", ",", "=", "::", "and", "or"]
	, P.reservedNames = ["true", "false", 
						 "if", "then", "elsif", "unless", "select", "case", 
						 "define", "include", "class", "node", "inherits"] 
	}

lexer = P.makeTokenParser puppetDef

m_parens = P.parens lexer
m_braces = P.braces lexer
m_brackets = P.brackets lexer
m_commaSep = P.commaSep lexer
m_integer = P.integer lexer
m_octal = P.octal lexer
m_hexadecimal = P.hexadecimal lexer
m_float = P.float lexer
m_stringLiteral = P.stringLiteral lexer
m_reserved = P.reserved lexer
m_whiteSpace = P.whiteSpace lexer
m_identifier = P.identifier lexer
m_colon = P.colon lexer
m_reservedOp = P.reservedOp lexer
m_symbol = P.symbol lexer

-- slightly hacky way to deal with case-insensitivity of resource types
resourceTypes = [ "Augeas", "File", "Exec", "Group", "Host", "Notify",
		"Package", "Resources", "Router", "Schedule", "Service",
                                 "User", "Stage", "Mount"]

normalizeResource :: String -> Maybe String
normalizeResource s = Data.List.find (\x -> ls == (map toLower x)) resourceTypes
  where ls = map toLower s

builtinResourceParser :: PuppetParser String
builtinResourceParser = do { x <- m_identifier
                           ; case normalizeResource x of
                               Just nr -> return nr
                               Nothing -> parserZero
                           } 

{------------------------------------------------------------------------------
    PuppetParser
------------------------------------------------------------------------------}

-- the PuppetParser uses the IO monad with ParsecT
-- this is necessary if you want to do IO in here
-- I'm not sure that we actually do at the moment,
-- but we'll leave it like this in case we want it later ...

type PuppetParser t = ParsecT [Char] CState IO t

-- the top-level PuppetParser
-- on success, return Right ast
-- on error, return Left with list of error messages

parsePuppet :: CState -> String -> IO (Either Errors AST)
parsePuppet st s = do
	result <- runParserT src st (srcPath $ sOpts st) s
	case (result) of
			Left err -> return $ Left [parseError err]
			Right ast -> return $ Right ast

{------------------------------------------------------------------------------
    a (top-level) configuration terminated by end-of-file
------------------------------------------------------------------------------}

src :: PuppetParser AST
src = do
	m_whiteSpace
	pos <- getPosition
	b <- program
	eof
	st <- getState
	return b

{------------------------------------------------------------------------------
    expressions
------------------------------------------------------------------------------}

table =
	[ --[Infix (m_reservedOp "?" >> return (Selector)) AssocLeft],
          [Prefix (m_reservedOp "!" >> return (Not))]
 	, [Infix (m_reservedOp "*" >> return (BinOps (TimOp))) AssocLeft, 
	   Infix (m_reservedOp "/" >> return (BinOps (DivOp))) AssocLeft]
	, [Infix (m_reservedOp "+" >> return (BinOps (AddOp))) AssocLeft, 
	   Infix (m_reservedOp "-" >> return (BinOps (MinOp))) AssocLeft]
	, [Infix (m_reservedOp ">" >> return (BinOps (GrtOp))) AssocLeft, 
	   Infix (m_reservedOp "<" >> return (BinOps (LessOp))) AssocLeft,
	   Infix (m_reservedOp ">=" >> return (BinOps (GeqOp))) AssocLeft, 
	   Infix (m_reservedOp "<=" >> return (BinOps (LeqOp))) AssocLeft]
	, [Infix (m_reservedOp "==" >> return (BinOps (EqOp))) AssocLeft,
	   Infix (m_reservedOp "!=" >> return (BinOps (UneqOp))) AssocLeft]
  , [Infix (m_reservedOp "and" >> return (BinOps (AndOp))) AssocLeft,
     Infix (m_reservedOp "or" >> return (BinOps (OrOp))) AssocLeft]
	]

term = m_parens expr 
    <|> (do { d <- m_integer ; return (DeRef (Values (ValueInt d))) })
    <|> (do { s <- m_stringLiteral ; return (DeRef (Values (ValueString s))) })
    <|> (do { r <- try builtinResourceParser
            ; try (do {d <- m_brackets m_identifier
                  ; ( do { att <- m_brackets expr 
                        ; return (DeRef (DeRefItem (Values (ValueRef r d)) att))})            
                  <|>return (DeRef (Values (ValueRef r d)))})
             <|> 
                  (do { e <- m_brackets (expr)
                      ; ( do { att <- m_brackets expr 
                        ; return (DeRef (DeRefItem (ResRef r e) att))})
                       <|> return (DeRef (ResRef r e))
                  })  
          })
    <|> (try (do { r <- qualIdentifier
                 ; e <- m_brackets (expr)
                      ; ( do { att <- m_brackets expr 
                        ; return (DeRef (DeRefItem (ResRef r e) att))})
                       <|> return (DeRef (ResRef r e))
                  }))
  
    <|> (do { s <- m_identifier 
            ;return (DeRef (Values (ValueString s)))
            })
    <|> (m_reserved "true" >> return (DeRef (Values (ValueBool True))))
    <|> (m_reserved "false" >> return (DeRef (Values (ValueBool False))))

    <|> do
        m_symbol "$"
        do
            m_reservedOp "::"
            path <- m_identifier `sepBy1` (m_reservedOp "::")
            let s = last path
            let classPath = intercalate "::" $ init path
            if (null classPath)
              then optionalArg $ ScopeVar STop s
              else optionalArg $ ScopeVar (SClass classPath) s
          <|> do
              s <- m_identifier
              optionalArg $ LocalVar s

    <|> (do { as <- m_brackets (m_commaSep expr)
            ; return (Array as)})
    <|> (do { as <- m_braces (m_commaSep hashEle)
            ; return (Hash as)})
{-    <|> try (do { v <- m_braces ( selectorBody )
            ; return (ListofPair v)} 
            )
  -}           

-- reference may be followed by optional (<expr>)


optionalArg :: Variable -> PuppetParser ValueExp
optionalArg v = do
        d <- many1 (m_brackets expr)
        return $ DeRef $ (foldl (DeRefItem) (Var v) d)
    <|> do return $ DeRef (Var v)
  

hashEle :: PuppetParser (Value, ValueExp)
hashEle = try (do { k <- m_integer
             ; m_reservedOp "=>"
             ; e <- expr  
             ; return ((ValueInt k), e)
             }
          <|>
          do { k <- m_identifier
             ; m_reservedOp "=>"
             ; e <- expr  
             ; return ((ValueString k), e)
             }
             )

{------------------------------------------------------------------------------
    top level PuppetParser
------------------------------------------------------------------------------}

program :: PuppetParser [ProgramEle]
program = many1 programEle 


expr :: PuppetParser ValueExp
expr = try (do { t <- term
               ; m_reservedOp "?"
               ; sbody <- m_braces (selectorBody)
               ; return (Selector t sbody)
               })
       <|> buildExpressionParser table term
              
names :: PuppetParser String
names = do { s <- m_identifier; return (s)}

--selector :: PuppetParser ValueExp
--selector = do { s <- m_identifier; }

selectorBody :: PuppetParser [(ValueExp, ValueExp)]
selectorBody = selectorEle `sepBy` (m_reservedOp ",")


selectorEle :: PuppetParser (ValueExp, ValueExp)
selectorEle = do { s1 <- expr 
           ; m_reservedOp "=>"
           ; s2 <- expr
           ; return ((s1, s2))}


ifCont :: PuppetParser IfCont
ifCont = ( do { m_reserved "elsif" 
                  ; e <- expr
                  ; s <- m_braces manyStatement
                  ; k <- optionMaybe ifCont 
                  ; return (Elseif e (StatementsList s) k)})
          <|> elseCont

elseCont :: PuppetParser IfCont
elseCont = ( do { m_reserved "else"
                  ; s <- m_braces manyStatement
                  ; return (Else (StatementsList s))})


statement :: PuppetParser Statements
statement = ( do { m_reserved "if"
                  ; e <- expr
                  ; s <- m_braces manyStatement
                  ; k <- optionMaybe ifCont
                  ; return (If e (StatementsList s) k)})
          <|> ( do { m_reserved "unless"
                  ; e <- expr
                  ; s <- m_braces manyStatement
                  ; k <- optionMaybe elseCont
                  ; return (Unless e (StatementsList s) k)
                  })
          <|> ( do { m_reserved "case"
                  ; e <- expr
                  ; s <- m_braces (caseBody)
                  ; return (Case e s)
                  })
          <|> ( do { m_symbol "$"
                    ; x <- m_identifier; 
                    ; do {m_reservedOp "="
                    ; e <- expr
                    ; return (Assignment x e) }
                    })
          <|> ( do { rn <- try builtinResourceParser;
                   ; (m_braces 
                              (do { e <- expr;
                                  ; m_reservedOp ":"
                                  ; b <- resourceBody
                                  ; return (Resource rn e b)})
                               )                
                    }) 
          <|> ( do { x <- qualIdentifier;
                    ; (m_braces
                          (do { e <- expr
                              ; m_reservedOp ":"
                              ; b <- resourceBody
                              ; return (ResTypeDecl x e b)
                              }))
                   })
          <|> ( do { m_reserved "include"
                  ; f <- qualIdentifier
                  ; return (Include f)
                  })
          <|> ( do { m_reserved "class"
                   ; m_braces ( do {n <- qualIdentifier
                                   ; m_colon
                                   ; r <- resourceBody
                                   ; return (ClassDecl n r)}) 
                   })
               
                


qualIdentifier :: PuppetParser String
qualIdentifier = do { ids <- m_identifier `sepBy1` (m_reservedOp "::")
                                 ; return (intercalate "::" ids)
                                 }

caseEle :: PuppetParser (ValueExp, Statements)
caseEle = do { e <- expr
             ; m_colon
             ; s <- m_braces manyStatement
             ; return (e, (StatementsList s))
             }

caseBody :: PuppetParser [(ValueExp, Statements)]
caseBody = many1 caseEle 


resourceBody :: PuppetParser [(String, ValueExp)]
resourceBody = resourceEle `sepBy` (m_reservedOp ",")

resourceEle :: PuppetParser (String, ValueExp)
resourceEle = do { x <- m_identifier
                 ; m_reservedOp "=>"
                 ; e <- expr
                 ; return (x, e) }


manyStatement :: PuppetParser [Statements]
manyStatement = many statement
                  

programEle :: PuppetParser ProgramEle
programEle = do { m_reserved "node"
                ; x<- m_identifier 
                ; s <- m_braces manyStatement
                ; return (Node x (StatementsList s))
                }
         <|> do { m_reserved "class"
                ; ( do { x <- qualIdentifier 
                       ; (do { p <- m_parens parameterList 
                         ; (( do { m_reserved "inherits"
                               ; y <- qualIdentifier
                               ; s <- m_braces manyStatement
                               ; return (Class x p (Just y) (StatementsList s)) 
                               })
                        <|> 
                          ( do { s <- m_braces manyStatement
                               ; return (Class x p Nothing (StatementsList s))
                               }))
                         })
                       <|>
                          (( do { m_reserved "inherits"
                               ; y <- qualIdentifier
                               ; s <- m_braces manyStatement
                               ; return (Class x [] (Just y) (StatementsList s)) 
                               }) 
                        <|> 
                          ( do { s <- m_braces manyStatement
                               ; return (Class x [] Nothing (StatementsList s))
                               }))
                     }
                   )
                <|> 
                  ( do { m_braces ( do {n <- m_identifier
                        ; m_colon
                        ; r <- resourceBody
                        ; return (ProStatement (ClassDecl n r)) } ) 
                        })         
                }  
         <|> do { m_reserved "define"
                ; x <- qualIdentifier
                ; pl <- optionMaybe (m_parens parameterList)
                ; s <- m_braces manyStatement
                ; case pl of
                    Just l -> return (DefResType x l (StatementsList s))
                    Nothing -> return (DefResType x [] (StatementsList s))}
         <|> do { s <- statement
                ; return (ProStatement s)
                }

-- a parameter (identifier) with an optional assigned value (expr)
parameter :: PuppetParser (String, Maybe ValueExp)
parameter = do
  m_reservedOp "$"
  x <- m_identifier
  e <- optionMaybe $ do
    m_reservedOp "="
    expr
  return (x, e)

-- a list of unique parameters (with optional values)
parameterList :: PuppetParser [(String, Maybe ValueExp)]
parameterList = do
  pvs <- parameter `sepBy` (m_reservedOp ",")
  case (checkUniqueParameters pvs) of
    Right pvs' -> do return pvs'
    Left msg -> fail msg

-- check list of parameter/values pairs for unique parameter names
checkUniqueParameters :: [(String, Maybe ValueExp)] -> Either String [(String, Maybe ValueExp)]
checkUniqueParameters [] = Right []
checkUniqueParameters (pv:pvs) = if elem (fst pv) (map fst pvs)
  then Left $ "duplicate parameter: " ++ (fst pv)
  else do
    pvs' <- checkUniqueParameters pvs
    return $ pv:pvs'
