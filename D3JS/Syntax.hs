{-# LANGUAGE OverloadedStrings,MultiParamTypeClasses,FlexibleInstances,ExistentialQuantification,TypeSynonymInstances #-}

module D3JS.Syntax where

import Data.Text (Text)
import qualified Data.Text as T

import D3JS.Type
import D3JS.Reify

import Control.Monad.RWS


-- Argument for data() function
class DataArg a
instance DataArg a => DataArg (Var' a)

instance DataArg JSObjArray
instance DataArg (Var' JSObjArray)
instance DataArg Data1D
instance DataArg FuncDef

-- | St (which means Statement) monad represents JavaScript statements.
-- "D3JS.Chart" uses St monad extensively.
type St r =  RWS () Text Int r

getUniqueNum :: St Int
getUniqueNum = do
	n <- get
	put (n+1)
	return n

instance Reifiable (St r) where
	reify st =
		let (a,s,w) = runRWS st () 0
		in w

-- Run a chain without assignment.
execute :: Chain () b -> St ()
execute chain = tell $ T.concat [reify chain,";\n"]

random_ :: NumFunc Double
random_ = ApplyFunc' "Math.random" []

class Assignable a where
	newVar :: St (Var' a)

	assign :: Chain () a -> St (Var' a)
	assign chain = do
		v@(Var' n) <- newVar
		tell $ T.concat ["var ",n," = ",reify chain,";\n"]
		return v

instance Assignable Data2D where
	newVar = newVar' "dat"

instance Assignable Data1D where
	newVar = newVar' "dat"

instance Assignable JSObjArray where
	newVar = newVar' "array"

instance Assignable (SelData Data2D) where
	newVar = newVar' "sel_dat"

instance Assignable (SelData JSObjArray) where
	newVar = newVar' "sel_array_obj"

instance Assignable Selection where
	newVar = newVar' "sel"

instance Assignable Scale where
	newVar = newVar' "scale"

instance Assignable Force where
	newVar = newVar' "force"

instance Assignable D3Func where
	newVar = newVar' "func"

instance Assignable Histogram where
	newVar = newVar' "hist"

newVar' :: Text -> St (Var' a)
newVar' baseName = getUniqueNum >>= (return . Var' . T.append baseName . show')

