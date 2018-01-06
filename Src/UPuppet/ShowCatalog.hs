{------------------------------------------------------------------------------
    uPuppet: Catalog rendering
------------------------------------------------------------------------------}

module UPuppet.ShowCatalog ( showCatalog ) where

import UPuppet.CState
import UPuppet.Catalog
import UPuppet.AST

import Data.List (intercalate)

-- render catalog as string

showCatalog :: CState -> Catalog -> String
showCatalog st catalog = concat $ map showResource catalog

showResource :: CatalogResource -> String
showResource (rType,rName,rAVs) =
	rType ++ " { " ++ (show rName)++ ":\n" ++
		(showResourceAVs rAVs) ++ "\n}"

showResourceAVs :: [(Attri, Value)] -> String
showResourceAVs rAVs = intercalate ",\n" $ map showResourceAV rAVs

showResourceAV :: (Attri, Value) -> String
showResourceAV (attr,value) =
	"  " ++ attr ++ " => " ++
		(showResourceValue value)

showResourceValue :: Value -> String
showResourceValue (ValueInt n) = show n
showResourceValue (ValueBool True) = "true"
showResourceValue (ValueBool False) = "false"
showResourceValue (ValueString s) = show s
showResourceValue (ValueFloat f) = show f
showResourceValue (ValueArray vs) = showResourceArray vs
showResourceValue (ValueHash vvs) = showResourceHash vvs
showResourceValue (ValueRef rn t) = rn ++ "['" ++ t ++ "']"

showResourceArray :: [Value] -> String
showResourceArray vs = "[" ++ (intercalate "," $ map showResourceValue vs) ++ "]"

showResourceHash :: [(Value,Value)] -> String
showResourceHash vvs = "{" ++ (intercalate ", " $ map showHashPair vvs) ++ "}"

showHashPair :: (Value,Value) -> String
showHashPair (k,v) = (showResourceValue k) ++ " => " ++ (showResourceValue v)
