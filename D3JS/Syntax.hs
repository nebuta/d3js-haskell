{-# LANGUAGE OverloadedStrings,MultiParamTypeClasses,FlexibleInstances,ExistentialQuantification,TypeSynonymInstances #-}

module D3JS.Syntax where

import Data.Text (Text)
import qualified Data.Text as T

import D3JS.Type
import D3JS.Reify

import Control.Monad.RWS

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

-- | d[0] as a user-defined function.
idx0 = Index 0 DataParam

-- | d[1] as a user-defined function.
idx1 = Index 1 DataParam

-- | d.x as a user-defined function.
_x = Field "x" DataParam

-- | d.y as a user-defined function.
_y = Field "y" DataParam

class Assignable a where
	newVar :: St (Var' a)

	assign :: Chain () a -> St (Var' a)
	assign chain = do
		v@(Var' n) <- newVar
		tell $ T.concat ["var ",n," = ",reify chain,";\n"]
		return v

instance Assignable Data2D where
	newVar = newVar' "dat"

instance Assignable Selection where
	newVar = newVar' "sel"

newVar' :: Text -> St (Var' a)
newVar' baseName = getUniqueNum >>= (return . Var' . T.append baseName . show')

