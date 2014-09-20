{-# LANGUAGE OverloadedStrings, GADTs, NoImplicitPrelude, ExistentialQuantification, FlexibleInstances, Rank2Types #-}

module D3JS.Type where

import Data.Text (Text)
import qualified Data.Text as T

import Prelude hiding ((.),id)
import Control.Category 

import Data.String

-- * Types

-- |This represents a method chain with an initial type of `a` and a final type of `b`
-- Chains are composable by functions in "Control.Category" module.
-- See "D3JS.Chart" for examples.
data Chain a b where
	Val :: Var -> Chain () b
	Val' :: (Reifiable b) => b -> Chain () b
	Val'' :: Var' b -> Chain () b
	-- Func :: JSFunc params b -> Chain a b
	Concat :: Chain c b -> Chain a c -> Chain a b
	Nil :: Chain a a
	-- ChainField :: Text -> Chain a b
	Refine :: JSObjClass a => Text -> Chain a b
	Apply :: forall a b params. [JSParam] -> Chain a (JSFunc params b) -> Chain a b

class JSObjClass a
instance JSObjClass JSObj
instance JSObjClass Force
instance JSObjClass Histogram
instance JSObjClass Selection
instance JSObjClass Transition
instance JSObjClass Scale

type ChainValue r = Chain () r
type IsoChain r = Chain r r

type RefineFunc = forall params r. Chain JSObj (JSFunc params r)
 
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

instance JSObjClass (SelData a)

-- Various objects
data Force = Force
data Scale = Scale
data Color = Color
data D3Func = D3Func -- functions returned by d3, e.g. d3.format, etc.

data Histogram = Histogram

-- The following types are just used as a tag for chaining functions with a type.
data Selection = Selection
data Transition = Transition

-- |Instances of Reifiable can generate a JavaScript code fragment.
class Reifiable a where
	reify :: a -> Text

-- |Used just as a tag for typing method chains. Used in "D3JS.Func".
class JSObjClass a => Sel a
instance Sel Selection
instance Sel (SelData a)


class JSArrayClass a

instance JSArrayClass Data1D
instance JSArrayClass Data2D
instance JSArrayClass JSObjArray

-- |Used just as a tag for typing method chains. Used in "D3JS.Func".
class JSObjClass a => Sel2 a
instance Sel2 Selection
instance Sel2 (SelData a)
instance (JSObjClass b) => Sel2 (Chain () b)
instance (JSObjClass a) => Sel2 (Var' a)
instance Sel2 Transition

instance JSObjClass JSObjArray

instance (JSObjClass a) => JSObjClass (Chain () a)
instance (JSObjClass a) => JSObjClass (Var' a)

-- * For internal use

-- | Function call for method chaining
data JSFunc params r = JSFunc FuncName [JSParam]  -- name and params

type FuncName = Text

-- | Parameter for a function call
data JSParam =
	ParamVar Var | PText Text | PDouble Double | PInt Int | PFunc FuncDef | forall r. PFunc' (NumFunc r)
	| PArray [JSParam] | PCompositeNum NumOp JSParam JSParam | forall r. PChainValue (ChainValue r)

data NumOp = forall a. Num a => NumOp (a -> a -> a) | NumOpAdd | NumOpSubt | NumOpMult | NumOpFromInteger -- stub

instance Num JSParam where
	a + b = PCompositeNum (NumOp (+)) a b 
	a - b = PCompositeNum (NumOp (-)) a b 
	a * b = PCompositeNum (NumOp (*)) a b 
	fromInteger a = PInt (fromIntegral a)

instance Fractional JSParam where
	fromRational n = PDouble (fromRational n)

-- | Function definition used for a callback.
data FuncDef = FuncTxt Text | forall r. FuncExp (NumFunc r)

instance IsString FuncDef where
	fromString = FuncTxt . T.pack

funcTxt :: Text -> JSParam
funcTxt = PFunc . FuncTxt

funcExp = PFunc . FuncExp

return_ :: NumFunc r -> FuncDef
return_ = FuncExp

-- | Representation of JavaScript function
-- Should be renamed to JSExp or something
data NumFunc r where
	NInt :: Int -> NumFunc Int
	NDouble :: Double -> NumFunc Double
	Add :: NumFunc r -> NumFunc r -> NumFunc r
	Subt :: NumFunc r -> NumFunc r -> NumFunc r
	Mult :: NumFunc r -> NumFunc r -> NumFunc r
	Div :: NumFunc r -> NumFunc r -> NumFunc r
	Mod :: NumFunc r -> NumFunc r -> NumFunc r
	Index :: NumFunc Int -> NumFunc [r] -> NumFunc r
	Field :: Text -> NumFunc JSObj -> NumFunc r
	Ternary :: NumFunc a -> NumFunc r -> NumFunc r -> NumFunc r
	NVar :: Var -> NumFunc r
	NVar' :: Var' r -> NumFunc r
	ChainVal :: ChainValue r -> NumFunc r
	DataParam :: NumFunc r
	DataIndex :: NumFunc Int
	DataIndexD :: NumFunc Double  -- ad hoc?
	ApplyFunc :: Var' a -> [JSParam] -> NumFunc r -- different from JSFunc, because this is not a method chain.
	ApplyFunc' :: FuncName -> [JSParam] -> NumFunc r -- different from JSFunc, because this is not a method chain.
	MkObject :: [(Text,NumFunc r)] -> NumFunc JSObject

mkObj = MkObject

data JSObj = JSObj

fieldd :: Text -> NumFunc JSObj -> NumFunc Double
fieldd = Field

infixl 7 ..>
(..>) :: NumFunc JSObj -> Text -> NumFunc a
a ..> b = Field b a

data JSObject = forall r. JSObject [(Text,NumFunc r)]

data JSObjArray = JSObjArray JSObject | JSObjArrayRaw Text -- internal representation is same as object. 

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

data SvgElement = Svg | Rect | Circle | Path | G | SvgOther Text
instance Show SvgElement where
	show Svg = "svg"
	show Rect = "rect"
	show Circle = "circle"
	show G = "g"
	show _ = "Unknown yet"
