{-# LANGUAGE OverloadedStrings, GADTs, NoImplicitPrelude, ExistentialQuantification, FlexibleInstances,OverlappingInstances #-}

module D3JS.Type where

import Data.Text (Text)
import qualified Data.Text as T

import Prelude hiding ((.),id)
import Control.Category 

-- * Types

-- |This represents a method chain with an initial type of `a` and a final type of `b`
-- Chains are composable by functions in "Control.Category" module.
-- See "D3JS.Chart" for examples.
data Chain a b where
	Val :: Var -> Chain () b
	Val' :: (Reifiable b) => b -> Chain () b
	Val'' :: Var' b -> Chain () b
	Func :: JSFunc a params b -> Chain a b
	Concat :: Chain c b -> Chain a c -> Chain a b
	Nil :: Chain a a
	ChainField :: Text -> Chain a b

-- | Chain a b behaves just like (a -> b).
-- Val Var is the starting point of chain (= constant),
-- and Nil is the termination of chain.
instance Category Chain where
	id = Nil
	(.) = Concat

type Var = Text

type Selector = Text

-- data D3Root = D3Root

data Data1D = Data1D [Double] deriving (Show)
data Data2D = Data2D [(Double,Double)] deriving (Show)

-- Selection with associated data
data SelData a = SelData

-- Various objects
data Force = Force
data Scale = Scale

-- The following types are just used as a tag for chaining functions with a type.
data Selection = Selection
data Transition = Transition

-- |Instances of Reifiable can generate a JavaScript code fragment.
class Reifiable a where
	reify :: a -> Text

-- |Used just as a tag for typing method chains. Used in "D3JS.Func".
class Sel a
instance Sel Selection
instance Sel (SelData a)

-- |Used just as a tag for typing method chains. Used in "D3JS.Func".
class Sel2 a
instance Sel2 Selection
instance Sel2 (SelData a)
instance Sel2 (Chain () b)
instance Sel2 (Var' a)



-- * For internal use

-- | Function call for method chaining
data JSFunc a c b = JSFunc FuncName [JSParam]  -- name and params

type FuncName = Text

-- | Parameter for a function call
data JSParam =
	ParamVar Var | PText Text | PDouble Double | PInt Int | PFunc FuncDef
	| PArray [JSParam]

-- | Function definition used for a callback.
data FuncDef = FuncTxt Text | forall r. FuncExp (NumFunc r)

funcTxt :: Text -> JSParam
funcTxt = PFunc . FuncTxt

funcExp = PFunc . FuncExp

-- | Representation of JavaScript function for a callback.
data NumFunc r where
	NInt :: Int -> NumFunc Int
	NDouble :: Double -> NumFunc Double
	Add :: NumFunc r -> NumFunc r -> NumFunc r
	Subt :: NumFunc r -> NumFunc r -> NumFunc r
	Mult :: NumFunc r -> NumFunc r -> NumFunc r
	Div :: NumFunc r -> NumFunc r -> NumFunc r
	Mod :: NumFunc r -> NumFunc r -> NumFunc r
	Index :: NumFunc Int -> NumFunc [r] -> NumFunc r
	Field :: Text -> NumFunc a -> NumFunc r
	Ternary :: NumFunc a -> NumFunc r -> NumFunc r -> NumFunc r
	NVar :: Var -> NumFunc r
	NVar' :: Var' r -> NumFunc r
	DataParam :: NumFunc r
	DataIndex :: NumFunc r
	ApplyFunc :: Var' a -> [JSParam] -> NumFunc r -- different from JSFunc, because this is not a method chain.
	ApplyFunc' :: FuncName -> [JSParam] -> NumFunc r -- different from JSFunc, because this is not a method chain.
	MkObject :: [(Text,NumFunc r)] -> NumFunc JSObject

data JSObject = forall r. JSObject [(Text,NumFunc r)]

class NumFuncVal a
instance NumFuncVal Int
instance NumFuncVal Double

instance Num (NumFunc Int) where
	fromInteger = NInt . (fromIntegral :: Integer -> Int)
	(+) = Add
	(-) = Subt
	(*) = Mult
	abs a = ApplyFunc' "abs" [funcExp a]
	signum a = ApplyFunc' "abs" [funcExp a]

instance Num (NumFunc Double) where
	fromInteger = NDouble . (fromIntegral :: Integer -> Double)
	(+) = Add
	(-) = Subt
	(*) = Mult
	abs a = ApplyFunc' "abs" [funcExp a]

instance Fractional (NumFunc Double) where
	(/) =  Div
	fromRational = NDouble . fromRational 


(%) :: NumFunc r -> NumFunc r -> NumFunc r
(%) = Mod

-- |This should not be used directly by users. Users should use 'assign' to get a variable instead.
data Var' dat = Var' {unVar' :: Var}  -- typed variables

