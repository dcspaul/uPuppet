{------------------------------------------------------------------------------
    uPuppet: Evaluation
------------------------------------------------------------------------------}

module UPuppet.Eval ( evalPuppet ) where

import Data.List
import Data.String.Utils
import Debug.Trace

import Text.Regex
import Text.Regex.PCRE

import UPuppet.CState
import UPuppet.AST
import UPuppet.Catalog
import UPuppet.Options

{------------------------------------------------------------------------------
    Types private to the evaluation
------------------------------------------------------------------------------}
-- define the type of the variable environment
type Env = [(Scope, Name, Value)]

-- look up a variable under some scope in the variable environment
lookupEnv :: Env -> Scope -> Name -> Maybe Value
lookupEnv [] _ _                                      = Nothing
lookupEnv ((s, n, v):es) sco x | (x == n && sco == s) = (Just v)
                               | otherwise = (lookupEnv es sco x) 

-- clear the elements in the environment associated with some specific scope
clearScope :: Scope -> Env -> Env
clearScope sco [] = []
clearScope sco ((s,n,v):es) | s == sco = clearScope sco es
                            | otherwise = (s,n,v):clearScope sco es

-- define the definitions
data Def = ClassDef (Maybe Name) OptParameterList Statements 
         | DeclaredClass (Scope)
         | ResTypeDef Name OptParameterList Statements
         deriving (Show)

-- define the type for the definition environment         
type DefEnv = [(Name, Def)]

-- the parent scope of a current scope for dereferencing
parentof :: DefEnv -> Scope -> Scope
parentof defEnv sco =  case sco of 
                          SClass b -> (lookupDefEnv defEnv b)
                          SNode    -> STop
                          STop     -> error "Top scope: No higher scope"
                          SDef b   -> baseof defEnv b

-- the base scope (toplevel or node) in effect in a given scope
baseof :: DefEnv -> Scope -> Scope
baseof defEnv STop        = STop
baseof defEnv SNode       = SNode
baseof defEnv (SDef sco)  = baseof defEnv sco
baseof defEnv (SClass a)  = baseof defEnv (lookupDefEnv defEnv a)

-- look up the variables in the variable environment with respect to the parent scope
lookforVar :: Env -> DefEnv -> Scope -> Variable -> Value
-- when the variable is a local variable
lookforVar es defEnv sco (LocalVar x) = case (lookupEnv es sco x) of 
                                        (Just b) -> b
                                        Nothing  -> case sco of STop -> (error ("unqualified variable not found in any scope: " ++ show x))
                                                                sco -> (lookforVar es defEnv (parentof defEnv sco) (LocalVar x))
-- when the variable is a variable with a scope
lookforVar es defEnv sco (ScopeVar sco' x) = case (lookupEnv es sco' x) of 
                    (Just b) -> b
                    Nothing  -> error ("lookForVar: " ++ (show sco') ++ " :: " ++ (show x))

-- create an environment from a list of string and value pairs and a scope
extendEnv :: Scope -> [(String, ValueExp)] -> Env
extendEnv _ [] = []
extendEnv sco ((x, (DeRef (Values y))):ys) = (sco, x, y):(extendEnv sco ys)

-- change the status of a class in the definition environment to "Declared"
changeDef :: DefEnv -> String -> Scope -> DefEnv
changeDef ((n, def):ds) a sco | n /= a = (n, def):(changeDef ds a sco) 
                              | n == a = (n, (DeclaredClass sco)):ds


-- look up the definiton environment for the parent class of a class
lookupDefEnv :: DefEnv -> Name -> Scope
lookupDefEnv [] b                     = STop 
lookupDefEnv ((a, def):ds) b | a == b = case def of 
                                        DeclaredClass sco -> sco
                             | a /= b = (lookupDefEnv ds b) 

-- loop up the status of a class in the definition environment
lookupDef :: (Eq a, Show a) => a -> [(a, b)] -> b
lookupDef a []                           = error ("lookupDef: cannot find " ++ show a)
lookupDef a ((name, v):ds) | (a == name) = v 
                           | a /= name   = (lookupDef a ds)

-- check whether a class is in the definition environment
isDef :: DefEnv -> String -> Bool
isDef [] _ = False
isDef ((x, def):ds) n = if x == n then True else isDef ds n 

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
type States a = (Env, DefEnv, Catalog, a)  

{------------------------------------------------------------------------------
    Evaluation of expressions of muPuppet
------------------------------------------------------------------------------}

evalExp :: States ValueExp -> Scope -> ValueExp
-- evaluate the variables 
-- it corresponds to the rules LVar, PVar, TVar and Qvar. 
-- Function lookforVar looks up the variables in the environment under the scope with respect to the parent scope
evalExp (env, defEnv, cv, (DeRef (Var x))) sco                                                                   = (DeRef (Values (lookforVar env defEnv sco x)))
-- evaluate the sum of two integer numbers 
-- it corresponds to the rule ARITHValue
evalExp (env, defEnv, cv, (BinOps AddOp (DeRef (Values (ValueInt x))) (DeRef (Values (ValueInt y))))) sco        = (DeRef (Values (ValueInt (x + y))))
-- evaluate the sum of two float numbers
evalExp (env, defEnv, cv, (BinOps AddOp (DeRef (Values (ValueFloat x))) (DeRef (Values (ValueFloat y))))) sco    = (DeRef (Values (ValueFloat (x + y))))
-- evaluate the minus of two integer numbers
evalExp (env, defEnv, cv, (BinOps MinOp (DeRef (Values (ValueInt x))) (DeRef (Values (ValueInt y))))) sco        = (DeRef (Values (ValueInt (x - y))))
-- evaluate the minus of two float numbers
evalExp (env, defEnv, cv, (BinOps MinOp (DeRef (Values (ValueFloat x))) (DeRef (Values (ValueFloat y))))) sco    = (DeRef (Values (ValueFloat (x - y))))
-- evaluate the multiplication of two integer numbers
evalExp (env, defEnv, cv, (BinOps TimOp (DeRef (Values (ValueInt x))) (DeRef (Values (ValueInt y))))) sco        = (DeRef (Values (ValueInt (x * y))))
-- evaluate the multiplication of two float numbers
evalExp (env, defEnv, cv, (BinOps TimOp (DeRef (Values (ValueFloat x))) (DeRef (Values (ValueFloat y))))) sco    = (DeRef (Values (ValueFloat (x * y))))
-- evaluate the division of two integer numbers
evalExp (env, defEnv, cv, (BinOps DivOp (DeRef (Values (ValueInt x))) (DeRef (Values (ValueInt y))))) sco        = (DeRef (Values (ValueInt (x `div` y))))
-- evaluate the division of two float numbers
evalExp (env, defEnv, cv, (BinOps DivOp (DeRef (Values (ValueFloat x))) (DeRef (Values (ValueFloat y))))) sco    = (DeRef (Values (ValueFloat (x / y))))
-- evaluate the modulo of two integer numbers
evalExp (env, defEnv, cv, (BinOps ModOp (DeRef (Values (ValueInt x))) (DeRef (Values (ValueInt y))))) sco        = (DeRef (Values (ValueInt (x `mod` y))))
-- evaluate the modulo of two float numbers
evalExp (env, defEnv, cv, (BinOps ModOp (DeRef (Values (ValueFloat _))) (DeRef (Values (ValueFloat _))))) sco    = error "Operation % not supported on floats!"
-- evaluate the "and" operation of two boolean values 
-- it corresponds to the rule ANDValue
evalExp (env, defEnv, cv, (BinOps AndOp (DeRef (Values (ValueBool x))) (DeRef (Values (ValueBool y))))) sco      = (DeRef (Values (ValueBool (x && y))))
-- evaluate the "or" operation of two boolean values 
evalExp (env, defEnv, cv, (BinOps OrOp  (DeRef (Values (ValueBool x))) (DeRef (Values (ValueBool y))))) sco      = (DeRef (Values (ValueBool (x || y))))
-- evaluate the "not" operation on the value "Ture" 
-- it corresponds to the rule NOTValueI
evalExp (env, defEnv, cv, (Not (DeRef (Values (ValueBool True))))) sco                                           = (DeRef (Values (ValueBool False)))
-- evaluate the "not" operation on the value "False" 
-- it corresponds to the rule NOTValueII
evalExp (env, defEnv, cv, (Not (DeRef (Values (ValueBool False))))) sco                                          = (DeRef (Values (ValueBool True)))
-- evaluate the ">" operation on two integer numbers
-- it corresponds to the rules COMPValueI and COMPValueII 
evalExp (env, defEnv, cv, (BinOps GrtOp (DeRef (Values (ValueInt x))) (DeRef (Values (ValueInt y))))) sco        = (DeRef (Values (ValueBool (x > y))))
-- evaluate the ">" operation on two float numbers
evalExp (env, defEnv, cv, (BinOps GrtOp (DeRef (Values (ValueFloat x))) (DeRef (Values (ValueFloat y))))) sco    = (DeRef (Values (ValueBool (x > y))))
-- evaluate the "<" operation on two integer numbers
evalExp (env, defEnv, cv, (BinOps LessOp (DeRef (Values (ValueInt x))) (DeRef (Values (ValueInt y))))) sco       = (DeRef (Values (ValueBool (x < y))))
-- evaluate the "<" operation on two float numbers
evalExp (env, defEnv, cv, (BinOps LessOp (DeRef (Values (ValueFloat x))) (DeRef (Values (ValueFloat y))))) sco   = (DeRef (Values (ValueBool (x < y))))
-- evaluate the ">=" operation on two integer numbers
evalExp (env, defEnv, cv, (BinOps GeqOp (DeRef (Values (ValueInt x))) (DeRef (Values (ValueInt y))))) sco        = (DeRef (Values (ValueBool (x >= y))))
-- evaluate the ">=" operation on two float numbers
evalExp (env, defEnv, cv, (BinOps GeqOp (DeRef (Values (ValueFloat x))) (DeRef (Values (ValueFloat y))))) sco    = (DeRef (Values (ValueBool (x >= y))))
-- evaluate the "<=" operation on two integer numbers
evalExp (env, defEnv, cv, (BinOps LeqOp (DeRef (Values (ValueInt x))) (DeRef (Values (ValueInt y))))) sco        = (DeRef (Values (ValueBool (x <= y))))
-- evaluate the "<=" operation on two float numbers
evalExp (env, defEnv, cv, (BinOps LeqOp (DeRef (Values (ValueFloat x))) (DeRef (Values (ValueFloat y))))) sco    = (DeRef (Values (ValueBool (x <= y))))
-- evaluate the "==" operation on two integer numbers
evalExp (env, defEnv, cv, (BinOps EqOp (DeRef (Values (ValueInt x))) (DeRef (Values (ValueInt y))))) sco         = (DeRef (Values (ValueBool (x == y))))
-- evaluate the "==" operation on two float numbers
evalExp (env, defEnv, cv, (BinOps EqOp (DeRef (Values (ValueFloat x))) (DeRef (Values (ValueFloat y))))) sco     = (DeRef (Values (ValueBool (x == y))))
-- evaluate the "==" operation on two string values
evalExp (env, defEnv, cv, (BinOps EqOp (DeRef (Values (ValueString x))) (DeRef (Values (ValueString y))))) sco   = (DeRef (Values (ValueBool ((inStringVar env defEnv sco x) == (inStringVar env defEnv sco y)))))
-- evaluate the "==" operation on two boolean values
evalExp (env, defEnv, cv, (BinOps EqOp (DeRef (Values (ValueBool x))) (DeRef (Values (ValueBool y))))) sco       = (DeRef (Values (ValueBool (x == y))))
-- evaluate the "!=" operation on two integer values
evalExp (env, defEnv, cv, (BinOps UneqOp (DeRef (Values (ValueInt x))) (DeRef (Values (ValueInt y))))) sco       = (DeRef (Values (ValueBool (x /= y))))
-- evaluate the "!=" operation on two float values
evalExp (env, defEnv, cv, (BinOps UneqOp (DeRef (Values (ValueFloat x))) (DeRef (Values (ValueFloat y))))) sco   = (DeRef (Values (ValueBool (x /= y))))
-- evaluate the "!=" operation on two string values
evalExp (env, defEnv, cv, (BinOps UneqOp (DeRef (Values (ValueString x))) (DeRef (Values (ValueString y))))) sco = (DeRef (Values (ValueBool ((inStringVar env defEnv sco x) /= (inStringVar env defEnv sco y)))))
-- evaluate the "!=" operation on two boolean values
evalExp (env, defEnv, cv, (BinOps UneqOp (DeRef (Values (ValueBool x))) (DeRef (Values (ValueBool y))))) sco     = (DeRef (Values (ValueBool (x /= y))))
-- evaluate the "+"  operation of two arrays
evalExp (env, defEnv, cv, (BinOps AddOp (DeRef (Values (ValueArray x))) (DeRef (Values (ValueArray y))))) sco    = (DeRef (Values (ValueArray (x ++ y))))
-- evaluate the "Not" operation on an expression  
-- it corresponds to the rule NOTStep
evalExp (env, defEnv, cv, (Not exp)) sco                                                                         = (Not (evalExp (env, defEnv, cv, exp) sco))
-- evaluate the second argument of any binary operation of the operator belonging to BinOps
-- it corresponds to the rules ARITHRight, COMRight, ANDRightI, ANDRightII
evalExp (env, defEnv, cv, (BinOps op (DeRef (Values v)) exp')) sco                                               = BinOps op (DeRef (Values v)) (evalExp (env, defEnv, cv, exp') sco)
-- evaluate the first argument of any binary operation of the operator belonging to BinOps
-- it corresponds to the rules ARITHLeft, COMLeft, ANDLeft
evalExp (env, defEnv, cv, (BinOps op exp exp')) sco                                                              = BinOps op (evalExp (env, defEnv, cv, exp) sco) exp'
-- evaluate the control expression in a selector if it is not a value
-- it corresponds to the rule SControl
evalExp (env, defEnv, cv, (Selector s sbody)) sco | not(isVal s)                                                 = Selector (evalExp (env,defEnv, cv, s) sco) sbody
-- error message for seletor when there is no default value or no matches
-- it corresponds to the real Puppet error
evalExp (env, defEnv, cv, (Selector _  [])) sco                                                                  = error "No value returned by selector"
-- compares the control value to the cases, if the cases are not values, evaluates them.
-- it corresponds to the rules SChooseI, SChooseII and SCase
evalExp (env, defEnv, cv, (Selector s@(DeRef (Values val)) ((x,z):xs))) sco = 
  case x of (DeRef (Values (ValueString "default"))) -> z
            (DeRef (Values (ValueString str)))       -> if v == (ValueString (inStringVar env defEnv sco str)) then z else (Selector s xs)
            (DeRef (Values w))                       -> if v == w then z else (Selector s xs)
            _                                        -> Selector s ((e,z):xs)
                                                        where e = evalExp (env, defEnv, cv, x) sco
    where
        v = case val of
            (ValueString str) -> ValueString (inStringVar env defEnv sco str)
            _                 -> val
evalExp (env, defEnv, cv, (Selector _ _)) sco                                                                    = error "Selector"
-- error message for an empty array
evalExp (env, defEnv, cv, (Array [])) sco   = error "empty array"
-- check whether an array is an array value, if not, evaluate the array by function "toValueArray"
-- it corresponds to the rules ARRExp, ARREleI, ARREleII  
evalExp (env, defEnv, cv, (Array (as))) sco = if (valueArray as) then (DeRef (Values (ValueArray (toValueArray as)))) 
                                              else (Array (evalArray (env, defEnv, cv, as) sco))
-- error message for an empty hash                                             
evalExp (env, defEnv, cv, (Hash [])) sco    = error "empty hash"
-- check whether a hash is a hash value, if not, evaluate the hash by function "toValueHash"
-- it corresponds to the rules HAExp, HAEleI, HAEleII 
evalExp (env, defEnv, cv, (Hash hs)) sco    = if (valueHash hs) then (DeRef (Values (ValueHash (toValueHash hs))))
                                              else (Hash (evalHash (env, defEnv, cv, hs) sco))
-- evaluate the array and hash dereferences
-- it corresponds to the rules DEREFExp, DEREFIndex, DEREFArray, DEREFHash                                             
evalExp (env, defEnv, cv, (DeRef (DeRefItem x r))) sco = case x of 
    (Var var)               -> DeRef (DeRefItem (Values (lookforVar env defEnv sco var)) r)
    (Values (ValueArray s)) -> case r of 
                               (DeRef (Values (ValueInt x)))    -> (DeRef (deRefArray s x))
                               DeRef (Values x)                 -> error "evalDeRefArray"
                               _                                -> DeRef (DeRefItem x (evalExp (env, defEnv, cv, r) sco))
    (Values (ValueHash s))  -> case r of 
                               DeRef (Values x)        -> DeRef (deRefHash s x)
                               _                       -> DeRef (DeRefItem x (evalExp (env, defEnv, cv, r) sco))
    (Values (ValueRef a b)) -> case r of 
                               (DeRef (Values (ValueString x))) -> (lookupCat cv a b $ inStringVar env defEnv sco x) 
                               _                                -> error "evalValueRef"             
    (ResRef a b)            -> case b of 
                               (DeRef (Values (ValueString x))) -> (DeRef (DeRefItem (Values (ValueRef a $ inStringVar env defEnv sco x)) r))
                               (DeRef (Values _ ))              -> error "evalResRef"
                               _                                -> (DeRef (DeRefItem (ResRef a (evalExp (env, defEnv, cv, b) sco)) r))
    (DeRefItem a b)         -> case (evalExp (env, defEnv, cv, (DeRef x)) sco) of
                               (DeRef y) -> (DeRef (DeRefItem y r))
    _                       -> error "evalExp1"

-- evaluate the resource dereference
-- it corresponds to the rules REFRes and DEREFRes 
evalExp (env, defEnv, cv, (DeRef (ResRef r n))) sco = case n of 
    (DeRef (Values (ValueString x))) -> (DeRef (Values (ValueRef r  $ inStringVar env defEnv sco x)))
    (DeRef (Values _ ))              -> error "ResRef1"
    _                                -> (DeRef (ResRef r (evalExp (env, defEnv, cv, n) sco)))

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
valueHash :: [(Value, ValueExp)] -> Bool
valueHash []                    = True
valueHash ((a, (DeRef (Values b))):hs) = valueHash hs 
valueHash _                     = False

-- convert a list of value and value expression pairs to a list of value and value pairs
toValueHash :: [(Value, ValueExp)] -> [(Value, Value)]
toValueHash []                    = []
toValueHash ((a, (DeRef (Values b))) : hs) = ((a, b):(toValueHash hs)) 

-- evaluate a list of expressions to a list of values            
evalArray :: States [ValueExp] -> Scope -> [ValueExp]
evalArray (_, _, _, []) sco                              = [] 
evalArray (env, defEnv, cv, ((DeRef (Values a)):as)) sco = ((DeRef (Values a)):(evalArray (env, defEnv, cv, as) sco))
evalArray (env, defEnv, cv, (a:as)) sco                  = (evalExp (env, defEnv, cv, a) sco):as 

-- evaluate a list of value and expression pairs to a list of value and value expression pairs  
evalHash :: States [(Value, ValueExp)] -> Scope  -> [(Value, ValueExp)]
evalHash (_, _, _, []) sco                                   = []
evalHash (env, defEnv, cv, ((x, (DeRef (Values h))):hs)) sco = ((x, (DeRef (Values h))):(evalHash (env, defEnv, cv, hs) sco))
evalHash (env, defEnv, cv, ((x,h):hs)) sco                   = (x, (evalExp (env, defEnv, cv, h) sco)):hs


{------------------------------------------------------------------------------
    Evaluation of the statements of muPuppet
------------------------------------------------------------------------------}

evalStat :: States Statements -> Scope -> States Statements
-- show error message when evaluating “Skip”
evalStat (env, defEnv, cv, Skip) sco                                 = error "evalStat1"
-- evaluate assignment statements 
-- it corresponds to the rules ASSIGN and ASSIGNStep
evalStat (env, defEnv, cv, (Assignment x y)) sco  = case y of 
    (DeRef (Values v)) -> if lookupEnv env sco x /= Nothing then error ("Variable " ++ show x ++ " already defined in scope " ++ show sco)
                          else case v of
                            (ValueString str) -> ((env ++ [(sco, x, ValueString $ inStringVar env defEnv sco str)]), defEnv, cv, Skip)
                            _                 -> ((env ++ [(sco, x, v)]), defEnv, cv, Skip)
    _                  -> (env, defEnv, cv, (Assignment x (evalExp (env, defEnv, cv, y) sco)))
-- throw errors when if or unless statements have an empty body
evalStat (env, defEnv, cv, (If _ (StatementsList []) _)) sco = error ("If statement has an empty body!")
evalStat (env, defEnv, cv, (Unless _ (StatementsList []) _)) sco = error ("Unless statement has an empty body!")
-- evaluate "if" statement when the control expression is equal to "True"
-- it corresponds to the rule IFT
evalStat (env, defEnv, cv, (If (DeRef (Values (ValueBool True))) y k)) sco = (env, defEnv, cv, y)
-- evaluate "if" statement when the control expression is equal to "False"
-- it corresponds to the rule IFF
evalStat (env, defEnv, cv, (If (DeRef (Values (ValueBool False))) y k)) sco =
    case k of
       Nothing -> (env, defEnv, cv, Skip)
       Just (Elseif e s k) -> (env, defEnv, cv, If e s k)
       Just (Else s) -> (env, defEnv, cv, s)
-- evaluate "if" statement when the control expression is an expression
-- it corresponds to the rule IFStep      
evalStat (env, defEnv, cv, (If x y k)) sco =
  let e = evalExp (env, defEnv, cv, x) sco in (env, defEnv, cv, (If e y k))
-- evaluate "unless" statement when the control expression is "True"
-- it corresponds to the rule UNLESST   
evalStat (env, defEnv, cv, (Unless (DeRef (Values (ValueBool True))) s k)) sco =
    case k of
      Nothing -> (env, defEnv, cv, Skip)
      Just (Else s) -> (env, defEnv, cv, s)
      Just (Elseif _ _ _) -> error "evalStat: 'elsif' not allowed with 'unless'"
-- evaluate "unless" statement when the control expression is "False"
-- it corresponds to the rule UNLESSF         
evalStat (env, defEnv, cv, (Unless (DeRef (Values (ValueBool False))) s k)) sco = (env, defEnv, cv, s)
-- if the control value is not a boolean, show error message, corresponding to the error in the real Puppet
evalStat (env, defEnv, cv, (Unless (DeRef (Values v)) s k)) sco = error "evalStat: Test component of 'unless' is not a Boolean value!"
-- evaluate "unless" statement when the control expression is an expression
-- it corresponds to the rule UNLESSStep 
evalStat (env, defEnv, cv, (Unless e s k)) sco = (env, defEnv, cv, (Unless e2 s k))
    where e2 = evalExp (env, defEnv, cv, e) sco
-- evaluate "case" statement if there is no cases
-- it corresponds to the rule CASEDone
evalStat (env, defEnv, cv, (Case x [])) sco          = (env, defEnv, cv, Skip)
-- evaluate "case" statement if there are cases
-- the branches correspond to the rule CASEMatch, CASENoMatch, CASEStep2 and CASEStep1 respectively
evalStat (env, defEnv, cv, (Case x ((z, s):xs))) sco = case new_x of 
    (DeRef (Values y)) -> case z of 
                          (DeRef (Values (ValueString "default"))) -> (env, defEnv, cv, s)
                          (DeRef (Values (ValueString str))) -> if (y == (ValueString (inStringVar env defEnv sco str))) then (env, defEnv, cv, s) else (env, defEnv, cv, (Case x xs))
                          (DeRef (Values n)) -> if (y==n) then (env, defEnv, cv, s) else (env, defEnv, cv, (Case x xs))
                          _                  -> (env, defEnv, cv, (Case x ((e, s):xs))) 
                                                where e = evalExp (env, defEnv, cv, z) sco
    _                  -> (env, defEnv, cv, (Case e ((z, s):xs))) 
                          where e = evalExp (env, defEnv, cv, x) sco
        where 
            new_x = case x of
                (DeRef (Values (ValueString str))) -> (DeRef (Values (ValueString (inStringVar env defEnv sco str))))
                _                                  -> x
-- evaluate "resource" 
-- the branches correspond to the rules RESDecl, RESStepI, RESStepII, RESStepIII, RESTitle,                          
evalStat (env, defEnv, cv, (Resource x y rs)) sco = case y of 
    (DeRef (Values (ValueString n))) -> if (valueRes rs) then (env, defEnv, (extendCat cv (x,inStringVar env defEnv sco n,toValueRes rs) ), Skip)
                                        else (env, defEnv, cv, (Resource x y (evaltoListValue env defEnv cv sco rs)))
    (DeRef (Values _ ))              -> error "wrong type of resource name"
    _                                -> (env, defEnv, cv, (Resource x e rs)) 
                                         where e = evalExp (env, defEnv, cv, y) sco
-- evaluate "include" statement
-- the branches correspond to the rules for the different definitions of class "a" which are INCD, INCU, INCPD and INCPU
evalStat (env, defEnv, cv, (Include a)) sco = 
    case (lookupDef a defEnv) of
      (DeclaredClass _ )       -> (env, defEnv, cv, Skip) 
      (ClassDef Nothing p s)   -> (env, (changeDef defEnv a (baseof defEnv sco)), cv, (ScopeStat (SClass a) (ClassCont (mergeParams [] p) s)))
      (ClassDef (Just b) p s)  -> case (lookupDef b defEnv) of 
                                    (DeclaredClass _ ) -> (env, (changeDef defEnv a (SClass b)), cv, (ScopeStat (SClass a) (ClassCont (mergeParams [] p) s)))
                                    (ClassDef _ p s)   -> (env, defEnv, cv, (StatementsList [(Include b), (Include a)]))
-- evaluate the helping statement "classcont" in scope statement in muPuppet                                   
evalStat (env, defEnv, cv, (ClassCont p s)) sco = 
    ((env ++ (extendEnv sco (evaltoListValue env defEnv cv sco p))), defEnv, cv, s)
-- evaluate the resource-like class declarations
-- the branches correspond to the rules CDecU, CDecPU and CDecPD
evalStat (env, defEnv, cv, (ClassDecl a as)) sco =  
    case (lookupDef a defEnv) of 
    (DeclaredClass _ ) -> error ("Duplicate declaration of class '" ++ a ++ "'")
    (ClassDef Nothing ps s) ->
       (env, (changeDef defEnv a (baseof defEnv sco)), cv,
        (ScopeStat (SClass a) (ClassCont (mergeParams as ps) s)))
    (ClassDef (Just b) ps s) ->
        case (lookupDef b defEnv) of 
          (DeclaredClass _) -> (env, (changeDef defEnv a (SClass b)), cv, (ScopeStat (SClass a) (ClassCont (mergeParams as ps) s)))
          (ClassDef _ _ _ ) -> (env, defEnv, cv, (StatementsList [(Include b), (ClassDecl a as)]))
-- evaluate the declaration of defined resource types
-- it corresponds to the rules DEF and DETStep
evalStat (env, defEnv, cv, (ResTypeDecl t title as)) sco
   | isVal title = 
    case (lookupDef t defEnv) of 
      (ResTypeDef t p s) -> if (valueRes as) 
                            then (env, defEnv, cv, (ScopeStat (SDef sco) (ResTypeCont t (("title",title):("name",title):mergeParams as p) s)))
                            else (env, defEnv, cv, (ResTypeDecl t title (evaltoListValue env defEnv cv sco as)))
      _                  -> error "resource is not defined"
   | otherwise = (env, defEnv, cv, (ResTypeDecl t (evalExp (env, defEnv, cv, title) sco) as))
-- evaluate the helping statement "ResTypeCont" in the scope statement for defined resource types
evalStat (env, defEnv, cv, (ResTypeCont t p s)) sco = 
    if (valueRes p) 
    then ((env ++ (extendEnv sco p)), defEnv, cv, s)
    else (env, defEnv, cv, (ResTypeCont t (evaltoListValue env defEnv cv sco p) s))
-- evaluate scope statements in muPuppet where the scope is a defined resource type and that reaches "Skip" statement
-- it corresponds to the rule DEFScopeDone
evalStat (env, defEnv, cv, (ScopeStat (SDef a) Skip)) sco = (clearScope (SDef a) env, defEnv, cv, Skip)
-- evaluate scope statements in muPuppet where the scope is "::", "::a" or "::nd" and that reaches "Skip" statement
-- it corresponds to the rule ScopeDone
evalStat (env, defEnv, cv, (ScopeStat a Skip)) sco = (env, defEnv, cv, Skip)
-- evaluate scope statements in muPuppet 
-- it corresponds to the rules ScopeStep and DEFScopeStep
evalStat (env, defEnv, cv, (ScopeStat sco' s)) sco =
  let (env', defEnv', cv', s') = (evalStat (env, defEnv, cv, s) sco') in (env', defEnv', cv', (ScopeStat sco' s'))
-- evaluate an empty list of statements in muPuppet to "Skip" statement
evalStat (env, defEnv, cv, (StatementsList [])) sco = (env, defEnv, cv, Skip)
-- evaluate a list of statements in muPuppet when the frist statement is "Skip" 
-- it corresponds to the rule SEQSkip
evalStat (env, defEnv, cv, (StatementsList (Skip:xs))) sco = (env, defEnv, cv, (StatementsList xs))         
-- evaluate a list of statements in muPuppet
-- it corresponds to the rule SEQStep
evalStat (env, defEnv, cv, (StatementsList (s:xs)))  sco = 
    let (env', defEnv', cv', s') = (evalStat (env, defEnv, cv, s) sco) in
    (env', defEnv', cv', (StatementsList (s':xs))) 

-- evaluate a list of string and expression pairs
evaltoListValue :: Env -> DefEnv -> Catalog -> Scope ->  [(String, ValueExp)] -> [(String, ValueExp)]
evaltoListValue _ _ _ _ [] = [] 
evaltoListValue env defEnv cv sco ((x,(DeRef (Values y))):ys) = ((x, (DeRef (Values y))):(evaltoListValue env defEnv cv sco ys))  
evaltoListValue env defEnv cv sco ((x,y):ys) = (x, (evalExp (env, defEnv, cv, y) sco)):(evaltoListValue env defEnv cv sco ys)

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

evalProgEle :: States ProgramEle -> Name -> States ProgramEle
-- evaluate the definition of nodes  
-- it corresponds to the rules NODEMatch and NODEnoMatch 
evalProgEle (env, defEnv, cv, (Node n s)) name | name == n = (env, defEnv, cv, ProStatement (ScopeStat SNode s))
                                               | otherwise = (env, defEnv, cv, ProSkip)
-- evaluate the definition of classes 
-- it covers the rules CDEF, CDEFI, CDEFP and CDEFPI                                                
evalProgEle (env, defEnv, cv, (Class a p b s)) name = 
    case (isDef defEnv a) of 
      False -> (env, (defEnv ++ [(a, (ClassDef b p s))]), cv, ProSkip)
      True  -> error "Class is defined"
-- evaluate the definition of defined resource types
-- it covers the rule RDEF        
evalProgEle (env, defEnv, cv, (DefResType t p s)) name = 
    case (isDef defEnv t) of 
      False -> (env, (defEnv ++ [(t, (ResTypeDef t p s))]), cv, ProSkip)
      True  -> error "Resource type is defined"
-- evaluate the end of evaluation of a statement at the program level to ProSkip statement      
evalProgEle (env, defEnv, cv, (ProStatement Skip)) name = 
    (env, defEnv, cv, ProSkip)
-- use the evaluation for statements to evaluate a statement at the program level
-- it corresponds to the rule TopScope    
evalProgEle (env, defEnv, cv, (ProStatement s)) name = 
    let (env', defEnv', cv', s') = (evalStat (env, defEnv, cv, s) STop)
    in (env', defEnv', cv', (ProStatement s'))

{------------------------------------------------------------------------------
    Evaluation of a program in muPuppet (a manifest in Puppet) 
------------------------------------------------------------------------------}

evalProg :: States Program -> Name -> States Program
-- evaluate an empty program to an empty program
evalProg (env, defEnv, cv, []) n = (env, defEnv, cv, [])
-- evaluate a list of elements of a program when the first element is "ProSkip"
-- it corresponds to the rule MSEQSkip
evalProg (env, defEnv, cv, (ProSkip:ps)) n = (env, defEnv, cv, ps)
-- evaluate a list of elements of a program
-- it corresponds to the rule MSEQStep
evalProg (env, defEnv, cv, (p:ps)) n = let (env', defEnv', cv', p') = (evalProgEle (env, defEnv, cv, p) n) in (env', defEnv', cv', (p':ps))

{------------------------------------------------------------------------------
    Evaluate AST for Puppet program & return the catalog
------------------------------------------------------------------------------}

evalPuppet :: CState -> AST -> IO (Either [String] Catalog)
evalPuppet st raw_ast = do { return (Right catalog') } where

	-- evaluate in steps
	(env', defEnv', catalog', ast') = 
		evalNSteps steps (env, defEnv, catalog, ast)

	-- initial values
	env = []
	defEnv = []
	catalog = []
	name = nodeName $ sOpts st
	steps = stepLimit $ sOpts st
	showTrace = (verbosity $ sOpts st) /= Normal
        ast = case mainClass $ sOpts st of
                Nothing -> raw_ast
                Just main -> raw_ast ++ [ProStatement (Include main)]

	-- evaluate steps with a trace of each step (if showTrace true)
	evalNSteps limit states@(_,_,_,ast) =
		if showTrace
			then trace (show ast) states' 
			else states'
		where states' = evalNSteps' limit states 

	-- evaluate steps
	-- stop when we program is reduced to a skip
	-- or stop when steplimit is reached
	evalNSteps' limit states@(_,_,_,ast) =
		case ast of
			-- reduced to a skip
			[] -> states
			-- check the limit
			otherwise -> case limit of
				Just 0 -> error ("Evaluation incomplete with result " ++ show states)
				Just n ->  evalNSteps (Just (n-1)) states'
				Nothing -> evalNSteps Nothing states'
		where
			states' = evalProg states name 
