{------------------------------------------------------------------------------
    uPuppet: Evaluation
------------------------------------------------------------------------------}

module UPuppet.Eval ( evalPuppet ) where

import Data.List
import Data.String.Utils
import Debug.Trace

import Text.Regex
import Text.Regex.PCRE
import Data.Char(toLower)
import Data.Bits(shiftL, shiftR)

import UPuppet.CState
import UPuppet.AST
import UPuppet.Catalog
import UPuppet.Options
import UPuppet.Scoping


-- check if main class exists in the AST
hasMain :: AST -> Bool
hasMain []            = False
hasMain (pro_stmt:ps) = case pro_stmt of
    Class x _ _ _ -> if x == "main" then True else hasMain ps
    _             -> hasMain ps

-- dereference in-string variables
inStringVar :: Env -> DefEnv -> Scope -> String -> String
inStringVar env defEnv sco text = substituteVars text vars vals
    where
      
      vars :: [String]
      vars = getAllTextMatches $ text =~ "(?<!\\\\)\\$(\\{\\w+\\}|\\w+)" :: [String]

      extractVal :: Maybe Value -> String
      extractVal x = case x of 
        (Just (ValueBool x))   -> if x then "true" else "false"
        (Just (ValueString x)) -> x
        (Just (ValueInt x))    -> show x
        (Just (ValueFloat x))  -> show x
        _                      -> ""
      
      nameOnly :: String -> String
      nameOnly (_:x:xs) = if x == '{' then init xs else x:xs
  
      vals :: [String]
      vals = [ extractVal $ lookupEnv env sco $ nameOnly x | x <- vars]

      replace' :: String -> (String, String) -> String
      replace' text (a,b) = replace a b text

      substituteVars :: String -> [String] -> [String] -> String
      substituteVars text [] []     = text
      substituteVars text vars vals = foldl replace' text $ zip vars vals

-- define the type of the states of a program in the process of evaluation
type States a = (Env, DefEnv, ScopedCatalog, a)  
type FullStates a = (CState, States a)

{------------------------------------------------------------------------------
    Evaluation of expressions of muPuppet
------------------------------------------------------------------------------}

evalExp :: FullStates ValueExp -> Scope -> ValueExp
-- evaluate the variables 
-- it corresponds to the rules LVar, PVar, TVar and Qvar. 
-- Function lookforVar looks up the variables in the environment under the scope with respect to the parent scope
evalExp (st, (env, defEnv, cv, (DeRef (Var x)))) sco                                                                   = (DeRef (Values (lookforVar st env defEnv sco x)))
-- evaluate the sum of two integer numbers 
-- it corresponds to the rule ARITHValue
evalExp (st, (env, defEnv, cv, (BinOps AddOp (DeRef (Values (ValueInt x))) (DeRef (Values (ValueInt y)))))) sco        = (DeRef (Values (ValueInt (x + y))))
-- evaluate the sum of two float numbers
evalExp (st, (env, defEnv, cv, (BinOps AddOp (DeRef (Values (ValueFloat x))) (DeRef (Values (ValueFloat y)))))) sco    = (DeRef (Values (ValueFloat (x + y))))
-- evaluate the minus of two integer numbers
evalExp (st, (env, defEnv, cv, (BinOps MinOp (DeRef (Values (ValueInt x))) (DeRef (Values (ValueInt y)))))) sco        = (DeRef (Values (ValueInt (x - y))))
-- evaluate the minus of two float numbers
evalExp (st, (env, defEnv, cv, (BinOps MinOp (DeRef (Values (ValueFloat x))) (DeRef (Values (ValueFloat y)))))) sco    = (DeRef (Values (ValueFloat (x - y))))
-- evaluate the multiplication of two integer numbers
evalExp (st, (env, defEnv, cv, (BinOps TimOp (DeRef (Values (ValueInt x))) (DeRef (Values (ValueInt y)))))) sco        = (DeRef (Values (ValueInt (x * y))))
-- evaluate the multiplication of two float numbers
evalExp (st, (env, defEnv, cv, (BinOps TimOp (DeRef (Values (ValueFloat x))) (DeRef (Values (ValueFloat y)))))) sco    = (DeRef (Values (ValueFloat (x * y))))
-- evaluate the division of two integer numbers
evalExp (st, (env, defEnv, cv, (BinOps DivOp (DeRef (Values (ValueInt x))) (DeRef (Values (ValueInt y)))))) sco        = (DeRef (Values (ValueInt (x `div` y))))
-- evaluate the division of two float numbers
evalExp (st, (env, defEnv, cv, (BinOps DivOp (DeRef (Values (ValueFloat x))) (DeRef (Values (ValueFloat y)))))) sco    = (DeRef (Values (ValueFloat (x / y))))
-- evaluate the modulo of two integer numbers
evalExp (st, (env, defEnv, cv, (BinOps ModOp (DeRef (Values (ValueInt x))) (DeRef (Values (ValueInt y)))))) sco        = (DeRef (Values (ValueInt (x `mod` y))))
-- evaluate the modulo of two float numbers
evalExp (st, (env, defEnv, cv, (BinOps ModOp (DeRef (Values (ValueFloat _))) (DeRef (Values (ValueFloat _)))))) sco    = error "Operation % not supported on floats!"
-- evaluate the "and" operation of two boolean values 
-- it corresponds to the rule ANDValue
evalExp (st, (env, defEnv, cv, (BinOps AndOp (DeRef (Values (ValueBool x))) (DeRef (Values (ValueBool y)))))) sco      = (DeRef (Values (ValueBool (x && y))))
-- evaluate the "or" operation of two boolean values 
evalExp (st, (env, defEnv, cv, (BinOps OrOp  (DeRef (Values (ValueBool x))) (DeRef (Values (ValueBool y)))))) sco      = (DeRef (Values (ValueBool (x || y))))
-- evaluate the "not" operation on the value "Ture" 
-- it corresponds to the rule NOTValueI
evalExp (st, (env, defEnv, cv, (UnaryOps Not (DeRef (Values (ValueBool True)))))) sco                                  = (DeRef (Values (ValueBool False)))
-- evaluate the "not" operation on the value "False" 
-- it corresponds to the rule NOTValueII
evalExp (st, (env, defEnv, cv, (UnaryOps Not (DeRef (Values (ValueBool False)))))) sco                                 = (DeRef (Values (ValueBool True)))
-- evaluate negating integer Values
evalExp (st, (env, defEnv, cv, (UnaryOps Negate (DeRef (Values (ValueInt x)))))) sco                                   = (DeRef (Values (ValueInt (-x))))
-- evaluate negating floating point Values
evalExp (st, (env, defEnv, cv, (UnaryOps Negate (DeRef (Values (ValueFloat x)))))) sco                                 = (DeRef (Values (ValueFloat (-x))))
-- substitute infix string variables
evalExp (st, (env, defEnv, cv, (UnaryOps InfixString (DeRef (Values (ValueString s)))))) sco                           = (DeRef (Values (ValueString (inStringVar env defEnv sco s))))
-- evaluate the ">" operation on two integer numbers
-- it corresponds to the rules COMPValueI and COMPValueII 
evalExp (st, (env, defEnv, cv, (BinOps GrtOp (DeRef (Values (ValueInt x))) (DeRef (Values (ValueInt y)))))) sco        = (DeRef (Values (ValueBool (x > y))))
-- evaluate the ">" operation on two float numbers
evalExp (st, (env, defEnv, cv, (BinOps GrtOp (DeRef (Values (ValueFloat x))) (DeRef (Values (ValueFloat y)))))) sco    = (DeRef (Values (ValueBool (x > y))))
-- evaluate the "<" operation on two integer numbers
evalExp (st, (env, defEnv, cv, (BinOps LessOp (DeRef (Values (ValueInt x))) (DeRef (Values (ValueInt y)))))) sco       = (DeRef (Values (ValueBool (x < y))))
-- evaluate the "<" operation on two float numbers
evalExp (st, (env, defEnv, cv, (BinOps LessOp (DeRef (Values (ValueFloat x))) (DeRef (Values (ValueFloat y)))))) sco   = (DeRef (Values (ValueBool (x < y))))
-- evaluate the ">=" operation on two integer numbers
evalExp (st, (env, defEnv, cv, (BinOps GeqOp (DeRef (Values (ValueInt x))) (DeRef (Values (ValueInt y)))))) sco        = (DeRef (Values (ValueBool (x >= y))))
-- evaluate the ">=" operation on two float numbers
evalExp (st, (env, defEnv, cv, (BinOps GeqOp (DeRef (Values (ValueFloat x))) (DeRef (Values (ValueFloat y)))))) sco    = (DeRef (Values (ValueBool (x >= y))))
-- evaluate the "<=" operation on two integer numbers
evalExp (st, (env, defEnv, cv, (BinOps LeqOp (DeRef (Values (ValueInt x))) (DeRef (Values (ValueInt y)))))) sco        = (DeRef (Values (ValueBool (x <= y))))
-- evaluate the "<=" operation on two float numbers
evalExp (st, (env, defEnv, cv, (BinOps LeqOp (DeRef (Values (ValueFloat x))) (DeRef (Values (ValueFloat y)))))) sco    = (DeRef (Values (ValueBool (x <= y))))
-- evaluate the "==" operation on two integer numbers
evalExp (st, (env, defEnv, cv, (BinOps EqOp (DeRef (Values (ValueInt x))) (DeRef (Values (ValueInt y)))))) sco         = (DeRef (Values (ValueBool (x == y))))
-- evaluate the "==" operation on two float numbers
evalExp (st, (env, defEnv, cv, (BinOps EqOp (DeRef (Values (ValueFloat x))) (DeRef (Values (ValueFloat y)))))) sco     = (DeRef (Values (ValueBool (x == y))))
-- evaluate the "==" operation on two string values
evalExp (st, (env, defEnv, cv, (BinOps EqOp (DeRef (Values (ValueString x))) (DeRef (Values (ValueString y)))))) sco   = (DeRef (Values (ValueBool (x == y))))
-- evaluate the "==" operation on two boolean values
evalExp (st, (env, defEnv, cv, (BinOps EqOp (DeRef (Values (ValueBool x))) (DeRef (Values (ValueBool y)))))) sco       = (DeRef (Values (ValueBool (x == y))))
-- evaluate the "!=" operation on two integer values
evalExp (st, (env, defEnv, cv, (BinOps UneqOp (DeRef (Values (ValueInt x))) (DeRef (Values (ValueInt y)))))) sco       = (DeRef (Values (ValueBool (x /= y))))
-- evaluate the "!=" operation on two float values
evalExp (st, (env, defEnv, cv, (BinOps UneqOp (DeRef (Values (ValueFloat x))) (DeRef (Values (ValueFloat y)))))) sco   = (DeRef (Values (ValueBool (x /= y))))
-- evaluate the "!=" operation on two string valuessubset
evalExp (st, (env, defEnv, cv, (BinOps UneqOp (DeRef (Values (ValueString x))) (DeRef (Values (ValueString y)))))) sco = (DeRef (Values (ValueBool (x /= y))))
-- evaluate the "!=" operation on two boolean values
evalExp (st, (env, defEnv, cv, (BinOps UneqOp (DeRef (Values (ValueBool x))) (DeRef (Values (ValueBool y)))))) sco     = (DeRef (Values (ValueBool (x /= y))))
-- evaluate the "+"  operation of two arrays
evalExp (st, (env, defEnv, cv, (BinOps AddOp (DeRef (Values (ValueArray x))) (DeRef (Values (ValueArray y)))))) sco    = (DeRef (Values (ValueArray (x ++ y))))
-- evaluate the "=~" operation of string and Regex
evalExp (st, (env, defEnv, cv, (BinOps Match (DeRef (Values (ValueString x))) (DeRef (Values (ValueRegex r)))))) sco   = (DeRef (Values (ValueBool (x =~ r))))
-- evaluate the "=~" operation of string and String
evalExp (st, (env, defEnv, cv, (BinOps Match (DeRef (Values (ValueString x))) (DeRef (Values (ValueString r)))))) sco  = (DeRef (Values (ValueBool (x =~ r))))
-- evaluate the "!~" operation of string and Regex
evalExp (st, (env, defEnv, cv, (BinOps NonMatch (DeRef (Values (ValueString x))) (DeRef (Values (ValueRegex r)))))) sco   = (DeRef (Values (ValueBool (not (x =~ r)))))
-- evaluate the "!~" operation of string and String
evalExp (st, (env, defEnv, cv, (BinOps NonMatch (DeRef (Values (ValueString x))) (DeRef (Values (ValueString r)))))) sco  = (DeRef (Values (ValueBool (not (x =~ r)))))
-- evaluate the "in" operation
evalExp (st, (env, defEnv, cv, (BinOps In (DeRef (Values x)) (DeRef (Values y))))) sco = case y of
    (ValueString s)  -> case x of
        (ValueString search) -> (DeRef (Values (ValueBool ((map toLower search) `isInfixOf` (map toLower s)))))
        (ValueRegex regex)   -> (DeRef (Values (ValueBool (s =~ regex))))
        _                    -> (DeRef (Values (ValueBool False)))
    (ValueArray arr) -> case x of
        (ValueString search) -> (DeRef (Values (ValueBool ((ValueString (map toLower search)) `elem` (map valueToLower arr)))))
        (ValueRegex regex)   -> (DeRef (Values (ValueBool (any (==True) (map (valueRegex regex) arr)))))
        _                    -> (DeRef (Values (ValueBool (x `elem` arr))))
    (ValueHash hash) -> case x of
        (ValueString search) -> (DeRef (Values (ValueBool ((ValueString (map toLower search)) `elem` (map valueToLower (fst (unzip hash)))))))
        (ValueRegex regex)   -> (DeRef (Values (ValueBool (any (==True) (map (valueRegex regex) (fst $ unzip hash))))))
        _                    -> (DeRef (Values (ValueBool (x `elem` (fst $ unzip hash)))))
    _                -> (DeRef (Values (ValueBool False)))
    where
        valueToLower :: Value -> Value
        valueToLower val = case val of
            (ValueString s) -> (ValueString $ map toLower s)
            _               -> val
        
        valueRegex :: String -> Value -> Bool
        valueRegex regex v = case v of
            (ValueString s) -> s =~ regex
            _               -> False

-- evaluate left bit shift
evalExp (st, (env, defEnv, cv, (BinOps LeftShift (DeRef (Values (ValueInt x))) (DeRef (Values (ValueInt y)))))) sco     = (DeRef (Values (ValueInt $ shiftL x (fromIntegral y))))
-- evaluate right bit shift
evalExp (st, (env, defEnv, cv, (BinOps RightShift (DeRef (Values (ValueInt x))) (DeRef (Values (ValueInt y)))))) sco     = (DeRef (Values (ValueInt $ shiftR x (fromIntegral y))))
-- evalExp (st, (env, defEnv, cv, (BinOps LeftShift ()))
-- it corresponds to the rule NOTStep
evalExp (st, (env, defEnv, cv, (UnaryOps op exp))) sco                                                                 = (UnaryOps op (evalExp (st, (env, defEnv, cv, exp)) sco))
-- evaluate the second argument of any binary operation of the operator belonging to BinOps
-- it corresponds to the rules ARITHRight, COMRight, ANDRightI, ANDRightII
evalExp (st, (env, defEnv, cv, (BinOps op (DeRef (Values v)) exp'))) sco                                               = BinOps op (DeRef (Values v)) (evalExp (st, (env, defEnv, cv, exp')) sco)
-- evaluate the first argument of any binary operation of the operator belonging to BinOps
-- it corresponds to the rules ARITHLeft, COMLeft, ANDLeft
evalExp (st, (env, defEnv, cv, (BinOps op exp exp'))) sco                                                              = BinOps op (evalExp (st, (env, defEnv, cv, exp)) sco) exp'
-- evaluate the control expression in a selector if it is not a value
-- it corresponds to the rule SControl
evalExp (st, (env, defEnv, cv, (Selector s sbody))) sco | not(isVal s)                                                 = Selector (evalExp (st, (env,defEnv, cv, s)) sco) sbody
-- error message for seletor when there is no default value or no matches
-- it corresponds to the real Puppet error
evalExp (st, (env, defEnv, cv, (Selector _  []))) sco                                                                  = error "No value returned by selector"
-- compares the control value to the cases, if the cases are not values, evaluates them.
-- it corresponds to the rules SChooseI, SChooseII and SCase
evalExp (st, (env, defEnv, cv, (Selector s@(DeRef (Values val)) ((x,z):xs)))) sco = 
  case x of (DeRef (Values (ValueString "default"))) -> z
            (UnaryOps Splat (DeRef (Values (ValueArray arr))))  -> evalExp (st, (env, defEnv, cv, (Selector s (e ++ xs)))) sco
              where e = map (\l -> ((DeRef (Values l)), z)) arr
            (DeRef (Values w))                       -> if val == w then z else (Selector s xs)
            _                                        -> Selector s ((e,z):xs)
                                                        where e = evalExp (st, (env, defEnv, cv, x)) sco
evalExp (st, (env, defEnv, cv, (Selector _ _))) sco                                                                    = error "Selector"
-- error message for an empty array
evalExp (st, (env, defEnv, cv, (Array []))) sco   = error "empty array"
-- check whether an array is an array value, if not, evaluate the array by function "toValueArray"
-- it corresponds to the rules ARRExp, ARREleI, ARREleII  
evalExp (st, (env, defEnv, cv, (Array (as)))) sco = if (valueArray as) then (DeRef (Values (ValueArray (toValueArray as)))) 
                                              else (Array (evalArray (st, (env, defEnv, cv, as)) sco))
-- error message for an empty hash                                             
evalExp (st, (env, defEnv, cv, (Hash []))) sco    = error "empty hash"
-- check whether a hash is a hash value, if not, evaluate the hash by function "toValueHash"
-- it corresponds to the rules HAExp, HAEleI, HAEleII 
evalExp (st, (env, defEnv, cv, (Hash hs))) sco    = if (valueHash hs) then (DeRef (Values (ValueHash (toValueHash hs))))
                                              else (Hash (evalHash (st, (env, defEnv, cv, hs)) sco))
-- evaluate the array and hash dereferences
-- it corresponds to the rules DEREFExp, DEREFIndex, DEREFArray, DEREFHash                                             
evalExp (st, (env, defEnv, cv, (DeRef (DeRefItem x r)))) sco = case x of 
    (Var var)               -> DeRef (DeRefItem (Values (lookforVar st env defEnv sco var)) r)
    (Values (ValueArray s)) -> case r of 
                               (DeRef (Values (ValueInt x)))    -> (DeRef (deRefArray s x))
                               DeRef (Values x)                 -> error "evalDeRefArray"
                               _                                -> DeRef (DeRefItem x (evalExp (st, (env, defEnv, cv, r)) sco))
    (Values (ValueHash s))  -> case r of 
                               DeRef (Values x)        -> DeRef (deRefHash s x)
                               _                       -> DeRef (DeRefItem x (evalExp (st, (env, defEnv, cv, r)) sco))
    (Values (ValueRef a b)) -> case r of 
                               (DeRef (Values (ValueString x))) -> (lookupCat cv a b x) 
                               _                                -> error "evalValueRef"             
    (ResRef a b)            -> case b of 
                               (DeRef (Values (ValueString x))) -> (DeRef (DeRefItem (Values (ValueRef a x)) r))
                               (DeRef (Values _ ))              -> error "evalResRef"
                               _                                -> (DeRef (DeRefItem (ResRef a (evalExp (st, (env, defEnv, cv, b)) sco)) r))
    (DeRefItem a b)         -> case (evalExp (st, (env, defEnv, cv, (DeRef x))) sco) of
                               (DeRef y) -> (DeRef (DeRefItem y r))
    _                       -> error "evalExp1"

-- evaluate the resource dereference
-- it corresponds to the rules REFRes and DEREFRes 
evalExp (st, (env, defEnv, cv, (DeRef (ResRef r n)))) sco = case n of 
    (DeRef (Values (ValueString x))) -> (DeRef (Values (ValueRef r x)))
    (DeRef (Values _ ))              -> error "ResRef1"
    _                                -> (DeRef (ResRef r (evalExp (st, (env, defEnv, cv, n)) sco)))

-- check whether an array is an array value
valueArray :: [ValueExp] -> Bool
valueArray []                    = True
valueArray ((DeRef (Values a)):as) = (valueArray as) 
valueArray _                     = False

-- convert a list of value expressions to a list of values
toValueArray :: [ValueExp] -> [Value]
toValueArray []                    = []
toValueArray ((DeRef (Values a)):as) = (a:(toValueArray as)) 

-- check whether an hash is an hash value
valueHash :: [(ValueExp, ValueExp)] -> Bool
valueHash []                    = True
valueHash (((DeRef (Values a)), (DeRef (Values b))):hs) = valueHash hs 
valueHash _                     = False

-- convert a list of value and value expression pairs to a list of value and value pairs
toValueHash :: [(ValueExp, ValueExp)] -> [(Value, Value)]
toValueHash []                                              = []
toValueHash (((DeRef (Values a)), (DeRef (Values b))) : hs) = ((a, b):(toValueHash hs)) 

-- evaluate a list of expressions to a list of values            
evalArray :: FullStates [ValueExp] -> Scope -> [ValueExp]
evalArray (_, (_, _, _, [])) sco                              = [] 
evalArray (st, (env, defEnv, cv, ((DeRef (Values a)):as))) sco = ((DeRef (Values a)):(evalArray (st, (env, defEnv, cv, as)) sco))
evalArray (st, (env, defEnv, cv, (a:as))) sco                  = (evalExp (st, (env, defEnv, cv, a)) sco):as 

-- evaluate a list of expression and expression pairs to a list of value expression and value expression pairs
evalHash :: FullStates [(ValueExp, ValueExp)] -> Scope  -> [(ValueExp, ValueExp)]
evalHash (_, (_, _, _, [])) sco                                                    = []
evalHash (st, (env, defEnv, cv, (((DeRef (Values x)), (DeRef (Values h))):hs))) sco = ((DeRef (Values x)), (DeRef (Values h))):(evalHash (st, (env, defEnv, cv, hs)) sco)
evalHash (st, (env, defEnv, cv, ((x, (DeRef (Values h))):hs))) sco                  = ((evalExp (st, (env, defEnv, cv, x)) sco), (DeRef (Values h))):(evalHash (st, (env, defEnv, cv, hs)) sco)
evalHash (st, (env, defEnv, cv, (((DeRef (Values x)),h):hs))) sco                   = ((DeRef (Values x)), (evalExp (st, (env, defEnv, cv, h)) sco)):(evalHash (st, (env, defEnv, cv, hs)) sco)
evalHash (st, (env, defEnv, cv, ((x,h):hs))) sco                                    = ((evalExp (st, (env, defEnv, cv, x)) sco), (evalExp (st, (env, defEnv, cv, h)) sco)):(evalHash (st, (env, defEnv, cv, hs)) sco)


{------------------------------------------------------------------------------
    Evaluation of the statements of muPuppet
------------------------------------------------------------------------------}

evalStat :: FullStates Statements -> Scope -> FullStates Statements
-- show error message when evaluating “Skip”
evalStat (st, (env, defEnv, cv, Skip)) sco                                 = error "evalStat1"
-- evaluate assignment statements 
-- it corresponds to the rules ASSIGN and ASSIGNStep
evalStat (st, (env, defEnv, cv, (Assignment x y))) sco  = case y of 
    (DeRef (Values v)) -> if lookupEnv env sco x /= Nothing then error ("Variable " ++ show x ++ " already defined in scope " ++ show sco)
                          else (st, ((env ++ [(sco, x, v)]), defEnv, cv, Skip))
    _                  -> (st, (env, defEnv, cv, (Assignment x (evalExp (st, (env, defEnv, cv, y)) sco))))
-- throw errors when if or unless statements have an empty body
evalStat (st, (env, defEnv, cv, (If _ (StatementsList []) _))) sco = error ("If statement has an empty body!")
evalStat (st, (env, defEnv, cv, (Unless _ (StatementsList []) _))) sco = error ("Unless statement has an empty body!")
-- evaluate "if" statement when the control expression is equal to "True"
-- it corresponds to the rule IFT
evalStat (st, (env, defEnv, cv, (If (DeRef (Values (ValueBool True))) y k))) sco = (st, (env, defEnv, cv, y))
-- evaluate "if" statement when the control expression is equal to "False"
-- it corresponds to the rule IFF
evalStat (st, (env, defEnv, cv, (If (DeRef (Values (ValueBool False))) y k))) sco =
    case k of
       Nothing -> (st, (env, defEnv, cv, Skip))
       Just (Elseif e s k) -> (st, (env, defEnv, cv, If e s k))
       Just (Else s) -> (st, (env, defEnv, cv, s))
-- evaluate "if" statement when the control expression is an expression
-- it corresponds to the rule IFStep      
evalStat (st, (env, defEnv, cv, (If x y k))) sco =
  let e = evalExp (st, (env, defEnv, cv, x)) sco in (st, (env, defEnv, cv, (If e y k)))
-- evaluate "unless" statement when the control expression is "True"
-- it corresponds to the rule UNLESST   
evalStat (st, (env, defEnv, cv, (Unless (DeRef (Values (ValueBool True))) s k))) sco =
    case k of
      Nothing -> (st, (env, defEnv, cv, Skip))
      Just (Else s) -> (st, (env, defEnv, cv, s))
      Just (Elseif _ _ _) -> error "evalStat: 'elsif' not allowed with 'unless'"
-- evaluate "unless" statement when the control expression is "False"
-- it corresponds to the rule UNLESSF         
evalStat (st, (env, defEnv, cv, (Unless (DeRef (Values (ValueBool False))) s k))) sco = (st, (env, defEnv, cv, s))
-- if the control value is not a boolean, show error message, corresponding to the error in the real Puppet
evalStat (st, (env, defEnv, cv, (Unless (DeRef (Values v)) s k))) sco = error "evalStat: Test component of 'unless' is not a Boolean value!"
-- evaluate "unless" statement when the control expression is an expression
-- it corresponds to the rule UNLESSStep 
evalStat (st, (env, defEnv, cv, (Unless e s k))) sco = (st, (env, defEnv, cv, (Unless e2 s k)))
    where e2 = evalExp (st, (env, defEnv, cv, e)) sco
-- evaluate "case" statement if there is no cases
-- it corresponds to the rule CASEDone
evalStat (st, (env, defEnv, cv, (Case x []))) sco          = (st, (env, defEnv, cv, Skip))
-- evaluate "case" statement if there are cases
-- the branches correspond to the rule CASEMatch, CASENoMatch, CASEStep2 and CASEStep1 respectively
evalStat (st, (env, defEnv, cv, (Case x ((z, s):xs)))) sco = case x of 
    (DeRef (Values y)) -> case z of
                          (UnaryOps Splat (DeRef (Values (ValueArray arr))))  -> (st, (env, defEnv, cv, (Case x (e ++ xs))))
                            where e = map (\v -> ((DeRef (Values v)), s)) arr
                          (DeRef (Values (ValueString "default"))) -> (st, (env, defEnv, cv, s))
                          (DeRef (Values n)) -> if (y==n) then (st, (env, defEnv, cv, s)) else (st, (env, defEnv, cv, (Case x xs)))
                          _                  -> (st, (env, defEnv, cv, (Case x ((e, s):xs))))
                                                where e = evalExp (st, (env, defEnv, cv, z)) sco
    _                  -> (st, (env, defEnv, cv, (Case e ((z, s):xs))))
                          where e = evalExp (st, (env, defEnv, cv, x)) sco
-- evaluate "resource" 
-- the branches correspond to the rules RESDecl, RESStepI, RESStepII, RESStepIII, RESTitle,                          
evalStat (st, (env, defEnv, cv, (Resource x y rs))) sco = case y of 
    (UnaryOps Splat v@(DeRef (Values (ValueArray _))))  -> (st, (env, defEnv, cv, (Resource x v rs)))  -- reduce splat as it behaves as an array would
    (DeRef (Values (ValueArray arr)))-> evalStat (st, (env, defEnv, cv, StatementsList (map (\v -> Resource x v rs) (map (\v -> (DeRef (Values v))) arr)))) sco -- split Resource into multiple
    (DeRef (Values (ValueString n))) -> if (valueRes rs) then (st, (env, defEnv, (updateCat cv (sco, (x, n,toValueRes rs)) defEnv), Skip))
                                        else (st, (env, defEnv, cv, (Resource x y (evaltoListValue st env defEnv cv sco rs))))
    (DeRef (Values _ ))              -> error "wrong type of resource name"
    _                                -> (st, (env, defEnv, cv, (Resource x e rs))) 
                                         where e = evalExp (st, (env, defEnv, cv, y)) sco
-- evaluate "include" statement
-- the branches correspond to the rules for the different definitions of class "a" which are INCD, INCU, INCPD and INCPU
evalStat (st, (env, defEnv, cv, (Include a))) sco = 
    case (lookupDef a defEnv) of
      (DeclaredClass _ )       -> (st, (env, defEnv, cv, Skip)) 
      (ClassDef Nothing p s)   -> (st, (env, (changeDef defEnv a (baseof defEnv sco)), cv, (ScopeStat (SClass a) (ClassCont (mergeParams [] p) s))))
      (ClassDef (Just b) p s)  -> case (lookupDef b defEnv) of 
                                    (DeclaredClass _ ) -> (st, (env, (changeDef defEnv a (SClass b)), cv, (ScopeStat (SClass a) (ClassCont (mergeParams [] p) s))))
                                    (ClassDef _ p s)   -> (st, (env, defEnv, cv, (StatementsList [(Include b), (Include a)])))
-- evaluate the helping statement "classcont" in scope statement in muPuppet                                   
evalStat (st, (env, defEnv, cv, (ClassCont p s))) sco = 
    (st, ((env ++ (extendEnv sco (evaltoListValue st env defEnv cv sco p))), defEnv, cv, s))
-- evaluate the resource-like class declarations
-- the branches correspond to the rules CDecU, CDecPU and CDecPD
evalStat (st, (env, defEnv, cv, (ClassDecl a as))) sco =  
    case (lookupDef a defEnv) of 
    (DeclaredClass _ ) -> error ("Duplicate declaration of class '" ++ a ++ "'")
    (ClassDef Nothing ps s) ->
       (st, (env, (changeDef defEnv a (baseof defEnv sco)), cv,
        (ScopeStat (SClass a) (ClassCont (mergeParams as ps) s))))
    (ClassDef (Just b) ps s) ->
        case (lookupDef b defEnv) of 
          (DeclaredClass _) -> (st, (env, (changeDef defEnv a (SClass b)), cv, (ScopeStat (SClass a) (ClassCont (mergeParams as ps) s))))
          (ClassDef _ _ _ ) -> (st, (env, defEnv, cv, (StatementsList [(Include b), (ClassDecl a as)])))
-- evaluate the declaration of defined resource types
-- it corresponds to the rules DEF and DETStep
evalStat (st, (env, defEnv, cv, (ResTypeDecl t title as))) sco
   | isVal title = 
    case (lookupDef t defEnv) of 
      (ResTypeDef t p s) -> if (valueRes as) 
                            then (st, (env, defEnv, cv, (ScopeStat (SDef sco) (ResTypeCont t (("title",title):("name",title):mergeParams as p) s))))
                            else (st, (env, defEnv, cv, (ResTypeDecl t title (evaltoListValue st env defEnv cv sco as))))
      _                  -> error "resource is not defined"
   | otherwise = (st, (env, defEnv, cv, (ResTypeDecl t (evalExp (st, (env, defEnv, cv, title)) sco) as)))
-- evaluate the helping statement "ResTypeCont" in the scope statement for defined resource types
evalStat (st, (env, defEnv, cv, (ResTypeCont t p s))) sco = 
    if (valueRes p) 
    then (st, ((env ++ (extendEnv sco p)), defEnv, cv, s))
    else (st, (env, defEnv, cv, (ResTypeCont t (evaltoListValue st env defEnv cv sco p) s)))
-- evaluate resource update Statements
evalStat (st, (env, defEnv, cv, (ResUpdate t (r:rs) b))) sco
    | isVal r = if not $ null rs then evalStat (st, (env, defEnv, cv', ResUpdate t (rs) b)) sco else (st, (env, defEnv, cv', Skip))
    | otherwise = evalStat (st, (env, defEnv, cv, (ResUpdate t ((evalExp (st, (env, defEnv, cv, r)) sco):rs) b))) sco
    where
        r' :: Name
        r' = case r of
            (DeRef(Values (ValueString x))) -> x
            _                               -> error ("Unexpected Resource Name")

        cv' :: ScopedCatalog
        cv' = updateCat cv (sco, (t, r', toValueRes b)) defEnv
-- evaluate scope statements in muPuppet where the scope is a defined resource type and that reaches "Skip" statement
-- it corresponds to the rule DEFScopeDone
evalStat (st, (env, defEnv, cv, (ScopeStat (SDef a) Skip))) sco = (st, (clearScope (SDef a) env, defEnv, cv, Skip))
-- evaluate scope statements in muPuppet where the scope is "::", "::a" or "::nd" and that reaches "Skip" statement
-- it corresponds to the rule ScopeDone
evalStat (st, (env, defEnv, cv, (ScopeStat a Skip))) sco = (st, (env, defEnv, cv, Skip))
-- evaluate scope statements in muPuppet 
-- it corresponds to the rules ScopeStep and DEFScopeStep
evalStat (st, (env, defEnv, cv, (ScopeStat sco' s))) sco =
  let (_, (env', defEnv', cv', s')) = (evalStat (st, (env, defEnv, cv, s)) sco') in (st, (env', defEnv', cv', (ScopeStat sco' s')))
-- evaluate an empty list of statements in muPuppet to "Skip" statement
evalStat (st, (env, defEnv, cv, (StatementsList []))) sco = (st, (env, defEnv, cv, Skip))
-- evaluate a list of statements in muPuppet when the frist statement is "Skip" 
-- it corresponds to the rule SEQSkip
evalStat (st, (env, defEnv, cv, (StatementsList (Skip:xs)))) sco = (st, (env, defEnv, cv, (StatementsList xs)))
-- evaluate a list of statements in muPuppet
-- it corresponds to the rule SEQStep
evalStat (st, (env, defEnv, cv, (StatementsList (s:xs))))  sco = 
    let (_, (env', defEnv', cv', s')) = (evalStat (st, (env, defEnv, cv, s)) sco) in
    (st, (env', defEnv', cv', (StatementsList (s':xs)))) 

-- evaluate a list of string and expression pairs
evaltoListValue :: CState -> Env -> DefEnv -> ScopedCatalog -> Scope ->  [(String, ValueExp)] -> [(String, ValueExp)]
evaltoListValue _ _ _ _ _ [] = [] 
evaltoListValue st env defEnv cv sco ((x,(DeRef (Values y))):ys) = ((x, (DeRef (Values y))):(evaltoListValue st env defEnv cv sco ys))  
evaltoListValue st env defEnv cv sco ((x,y):ys) = (x, (evalExp (st, (env, defEnv, cv, y)) sco)):(evaltoListValue st env defEnv cv sco ys)

-- check whether an expression is a value
isVal :: ValueExp -> Bool
isVal (DeRef (Values v)) = True
isVal _ = False

-- check whether a list of string and expression pairs is a list of string and value expression pairs
valueRes :: [(String, ValueExp)] -> Bool
valueRes []                           = True
valueRes ((x, (DeRef (Values v))):as) = valueRes as
valueRes _                            = False

-- convert a list of string and value expression pairs to a list of string and value pairs
toValueRes :: [(String, ValueExp)] -> [(String, Value)]
toValueRes []                           = []
toValueRes ((x, (DeRef (Values v))):as) = ((x, v):(toValueRes as))
toValueRes _                            = error "not all element is a value"

{------------------------------------------------------------------------------
    Evaluation of elements of a program of muPuppet 
------------------------------------------------------------------------------}

evalProgEle :: FullStates ProgramEle -> Name -> FullStates ProgramEle
-- evaluate the definition of nodes  
-- it corresponds to the rules NODEMatch and NODEnoMatch 
evalProgEle (st, (env, defEnv, cv, (Node n s))) name | name == n = (st, (env, defEnv, cv, ProStatement (ScopeStat SNode s)))
                                                     | otherwise = (st, (env, defEnv, cv, ProSkip))
-- evaluate the definition of classes 
-- it covers the rules CDEF, CDEFI, CDEFP and CDEFPI                                                
evalProgEle (st, (env, defEnv, cv, (Class a p b s))) name = 
    case (isDef defEnv a) of 
      False -> (st, (env, (defEnv ++ [(a, (ClassDef b p s))]), cv, ProSkip))
      True  -> error "Class is defined"
-- evaluate the definition of defined resource types
-- it covers the rule RDEF        
evalProgEle (st, (env, defEnv, cv, (DefResType t p s))) name = 
    case (isDef defEnv t) of 
      False -> (st, (env, (defEnv ++ [(t, (ResTypeDef t p s))]), cv, ProSkip))
      True  -> error "Resource type is defined"
-- evaluate the end of evaluation of a statement at the program level to ProSkip statement      
evalProgEle (st, (env, defEnv, cv, (ProStatement Skip))) name = 
    (st, (env, defEnv, cv, ProSkip))
-- use the evaluation for statements to evaluate a statement at the program level
-- it corresponds to the rule TopScope    
evalProgEle (st, (env, defEnv, cv, (ProStatement s))) name = 
    let (_, (env', defEnv', cv', s')) = (evalStat (st, (env, defEnv, cv, s)) STop)
    in (st, (env', defEnv', cv', (ProStatement s')))

{------------------------------------------------------------------------------
    Evaluation of a program in muPuppet (a manifest in Puppet) 
------------------------------------------------------------------------------}

evalProg :: FullStates Program -> Name -> FullStates Program
-- evaluate an empty program to an empty program
evalProg (st, (env, defEnv, cv, [])) n = (st, (env, defEnv, cv, []))
-- evaluate a list of elements of a program when the first element is "ProSkip"
-- it corresponds to the rule MSEQSkip
evalProg (st, (env, defEnv, cv, (ProSkip:ps))) n = (st, (env, defEnv, cv, ps))
-- evaluate a list of elements of a program
-- it corresponds to the rule MSEQStep
evalProg (st, (env, defEnv, cv, (p:ps))) n = let (_, (env', defEnv', cv', p')) = (evalProgEle (st, (env, defEnv, cv, p)) n) in (st, (env', defEnv', cv', (p':ps)))

{------------------------------------------------------------------------------
    Evaluate AST for Puppet program & return the catalog
------------------------------------------------------------------------------}

evalPuppet :: CState -> AST -> IO (Either [String] Catalog)
evalPuppet st raw_ast = do { return (Right $ scopedCatToCat catalog') } where

	-- evaluate in steps
	(env', defEnv', catalog', ast') = 
		evalNSteps steps (st, (env, defEnv, catalog, ast))

	-- initial values
	env = []
	defEnv = []
	catalog = []
	name = nodeName $ sOpts st
	steps = stepLimit $ sOpts st
	showTrace = (verbosity $ sOpts st) /= Normal
        ast = case mainClass $ sOpts st of
                Nothing -> raw_ast
                Just main -> if hasMain raw_ast then raw_ast ++ [ProStatement (Include main)] else raw_ast -- sanity check (does main exist)

	-- evaluate steps with a trace of each step (if showTrace true)
	evalNSteps limit full_state@(_, (_,_,_,ast)) =
		if showTrace
			then trace (show ast) states' 
			else states'
		where states' = evalNSteps' limit full_state 

	-- evaluate steps
	-- stop when we program is reduced to a skip
	-- or stop when steplimit is reached
	evalNSteps' limit full_state@(_, states@(_,_,_,ast)) =
		case ast of
			-- reduced to a skip
			[] -> states
			-- check the limit
			otherwise -> case limit of
				Just 0 -> error ("Evaluation incomplete with result " ++ show states)
				Just n ->  evalNSteps (Just (n-1)) states'
				Nothing -> evalNSteps Nothing states'
		where
			states' = evalProg full_state name 
