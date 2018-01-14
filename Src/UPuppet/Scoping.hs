module UPuppet.Scoping (Name, Env, Def(..), DefEnv,
 lookupEnv, clearScope, parentof, childof, baseof, lookforVar, extendEnv, changeDef,
 lookupDefEnv, lookupDef, isDef) where

import UPuppet.AST
import UPuppet.CState
import UPuppet.Options

-- This file contains all scoping related functionality and queries needed for evaluation
type Name = String
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

-- returns True if class is another class's child (they share a scope)
childof :: DefEnv -> Scope -> Scope -> Bool
childof defEnv (SClass a) (SClass b) = if a == b then False else childof' defEnv (lookupDefEnv defEnv a) (SClass b)
    where 
        childof' :: DefEnv -> Scope -> Scope -> Bool
        childof' defEnv (SClass a) (SClass b) = if a == b then True else childof' defEnv (lookupDefEnv defEnv a) (SClass b)
        childof' defEnv _ _                   = False
childof defEnv _ _  = False

-- look up the variables in the variable environment with respect to the parent scope
lookforVar :: CState -> Env -> DefEnv -> Scope -> Variable -> Value
-- when the variable is a local variable
lookforVar st es defEnv sco (LocalVar x) = case (lookupEnv es sco x) of 
                                        (Just b) -> b
                                        Nothing  -> case sco of STop -> if strictVariables $ sOpts st
                                                                        then error ("unqualified variable not found in any scope: " ++ show x)
                                                                        else Undef
                                                                sco -> (lookforVar st es defEnv (parentof defEnv sco) (LocalVar x))
-- when the variable is a variable with a scope
lookforVar _ es defEnv sco (ScopeVar sco' x) = case (lookupEnv es sco' x) of 
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
