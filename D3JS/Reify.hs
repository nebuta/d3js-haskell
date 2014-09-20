{-# LANGUAGE OverloadedStrings, GADTs, FlexibleInstances #-}

module D3JS.Reify where

import Data.Text (Text)
import qualified Data.Text as T

import D3JS.Type

instance Reifiable Var where
	reify t = t

instance Reifiable (Var' r) where
	reify (Var' name) = name

instance Reifiable (Chain a b) where
	reify (Val name) = name
	reify (Val' v) = reify v
	reify (Val'' (Var' n)) = n
	reify (Concat Nil g) = reify g
	reify (Concat f g) = T.concat [reify g,".",reify f] -- method chain flows from left to right, so flips f and g.
	-- reify (Func f) = reify f
	reify Nil = ""
	-- reify (ChainField name) = name
	reify (Refine name) = name
	reify (Apply args chain) = T.concat [reify chain,"(",T.intercalate "," $ map reify args,")"]

-- instance Reifiable D3Root where
--	reify D3Root = "d3"

instance Reifiable Data1D where
	reify (Data1D ps) = surround $ T.intercalate "," $ map show' ps

instance Reifiable Data2D where
	reify (Data2D ps) = surround $ T.intercalate "," $ map (\(x,y) -> T.concat ["[",show' x,",",show' y,"]"]) ps

instance Reifiable (JSFunc params r) where
	reify (JSFunc name params) = T.concat [name,"(",T.intercalate "," $ map reify params,")"]

instance Reifiable JSParam where
	reify (ParamVar name) = name
	reify (PText t) = T.concat ["\"",t,"\""]
	reify (PDouble d) = show' d
	reify (PInt d) = show' d
	reify (PFunc (FuncTxt t)) = t
	reify (PFunc (FuncExp f)) = T.concat["function(d,i){return ",reify f,";}"]
	reify (PFunc' f) = reify f
	reify (PArray vs) = T.concat ["[",T.intercalate "," $ map reify vs,"]"]
	reify (PChainValue v) = reify v


instance Reifiable (NumFunc r) where
	reify (NInt i) = show' i
	reify (NDouble d) = show' d
	reify (NVar v) = v
	reify (Add a b) = T.concat [reify a," + ",reify b]
	reify (Mult a b) = T.concat [reify a," * ",reify b]
	reify (Subt a b) = T.concat [reify a," - ",reify b]
	reify (Mod a b) = T.concat [reify a," % ",reify b]
	reify DataParam = "d"
	reify DataIndex = "i"
	reify DataIndexD = "i"
	reify (ChainVal chain) = reify chain
	reify (Index i ns) = T.concat [reify ns,"[",reify i,"]"]
	reify (Field name obj) = T.concat [reify obj,".",name]
	reify (Ternary cond a b) = T.concat ["(", reify cond, ") ? (", reify a, ") : (", reify b, ")"]
	reify (ApplyFunc var params) = T.concat [unVar' var,"(",T.intercalate "," $ map reify params,")"]
	reify (ApplyFunc' name params) = T.concat [name,"(",T.intercalate "," $ map reify params,")"]
	reify (MkObject pairs) =
		let f (key,val) = T.concat [key,": ",reify val]
		in T.concat ["{",T.intercalate "," $ map f pairs,"}"]

	-- Stub: incomplete!!


show' :: (Show a) => a -> Text
show' = T.pack . show

surround s = T.concat ["[",s,"]"]
