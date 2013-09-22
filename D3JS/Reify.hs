{-# LANGUAGE OverloadedStrings, GADTs, FlexibleInstances #-}

module D3JS.Reify where

import Data.Text (Text)
import qualified Data.Text as T

import D3JS.Type

instance Reifiable Var where
	reify t = t

instance Reifiable (Chain a b) where
	reify (Val name) = name
	reify (Val' v) = reify v
	reify (Val'' (Var' n)) = n
	reify (Concat f g) = T.concat [reify g,".",reify f]  -- method chain flows from left to right, so flips f and g.
	reify (Func f) = reify f
	reify Nil = ""

-- instance Reifiable D3Root where
--	reify D3Root = "d3"

instance Reifiable Data1D where
	reify (Data1D ps) = surround $ T.intercalate "," $ map show' ps

instance Reifiable Data2D where
	reify (Data2D ps) = surround $ T.intercalate "," $ map (\(x,y) -> T.concat ["[",show' x,",",show' y,"]"]) ps

instance Reifiable (JSFunc a c b) where
	reify (JSFunc name params) = T.concat [name,"(",T.intercalate "," $ map reify params,")"]

instance Reifiable JSParam where
	reify (ParamVar name) = name
	reify (PText t) = T.concat ["\"",t,"\""]
	reify (PDouble d) = show' d
	reify (PFunc (FuncTxt t)) = t
	reify (PFunc (FuncExp f)) = T.concat["function(d){return ",reify f,";}"]


instance Reifiable (NumFunc r) where
	reify DataParam = "d"
	reify (Index i ns) = T.concat [reify ns,"[",show' i,"]"]
	reify (Field name obj) = T.concat [reify obj,".",name]
	-- Stub: incomplete!!


show' :: (Show a) => a -> Text
show' = T.pack . show

surround s = T.concat ["[",s,"]"]
