{-# LANGUAGE OverloadedStrings #-}

-- |This module defines original functions of D3.js, as well as some low-level helper functions.
module D3JS.Func where

import D3JS.Type
import D3JS.Syntax

import Data.List
import Data.Text (Text)
import qualified Data.Text as T 

import Prelude hiding ((.),id)
import Control.Category

-- * Selection and data assignment

-- | d3 object in D3.js
d3Root :: Chain () Selection
d3Root = Val "d3"

use :: Var' r -> Chain () r
use = Val''

(.>) :: Var' r -> Chain r b -> Chain () b
v .> chain = Val'' v >>> chain

-- | select() in D3.js
select :: Selector -> Chain Selection Selection
select = funct1 "select"

-- | selectAll()
selectAll :: (JSObjClass a, Sel2 a) => Selector -> Chain a Selection
selectAll = funct1 "selectAll"

-- * Data manipulation

-- | data() in D3.js. Assigns new data to selection.
dataD3 :: DataArg r => Var' r -> Chain Selection (SelData r)
dataD3 (Var' d) = func "data" [ParamVar d]

dataD3' :: NumFunc r -> Chain Selection (SelData r)
dataD3' f = func "data" [funcExp f]

-- | insert()
insertD3 :: Text -> Chain (SelData a) (SelData a)
insertD3 = funct1 "append"

-- | enter()
enter :: Chain (SelData r) (SelData r)
enter = func "enter" []

-- | exit()
exit :: Chain (SelData r) (SelData r)
exit = func "exit" []

-- | remove()
remove :: Chain (SelData r) (SelData r)
remove = func "remove" []

datum :: Var' r -> Chain Selection (SelData r)
datum (Var' d) = func "datum" [ParamVar d]

-- | map()
mapD3 :: (JSArrayClass a,JSObjClass a) => NumFunc r -> Chain a JSObjArray -- stub
mapD3 f = func "map" [funcExp f]

mapD3' :: (JSArrayClass a,JSObjClass a) => Text -> Chain a JSObjArray -- stub
mapD3' ft = func "map" [funcTxt ft]

-- | filter()
filterD3 :: FuncDef -> Chain (SelData r) (SelData r)
filterD3 f = func "filter" [PFunc f]

-- | sort()
sortD3 :: FuncDef -> Chain (SelData r) (SelData r)
sortD3 f = func "sort" [PFunc f]

-- | order()
order :: Chain (SelData r) (SelData r)
order = func "order" []

-- | append()
appendD3 :: (JSObjClass a) => SvgElement -> Chain a a
appendD3 elem = funct1 "append" (T.pack . show $ elem)

-- | max()
maxD3 :: Var' b -> FuncDef -> Chain () JSObjArray -- stub
maxD3 (Var' v) f = d3Root >>> func "max" [ParamVar v, PFunc f]

-- * Attributes and styles

attr :: (Sel2 a) => Text -> JSParam -> Chain a a
attr key val = func' "attr" [PText key, val]

attrf :: (Sel2 a) => Text -> NumFunc r -> Chain a a
attrf key val = func' "attr" [PText key, funcExp val]

attrt :: (Sel2 a) => Text -> Text -> Chain a a
attrt key val = attr key (PText val)

attrd :: (Sel2 a) => Text -> Double -> Chain a a
attrd key val = attr key (PDouble val)

attrds :: (Sel2 a) => [(Text,Double)] -> Chain a a
attrds [] = id
attrds ((k,v):xs) = attrd k v >>> attrds xs

attri :: (Sel2 a) => Text -> Int -> Chain a a
attri key val = attr key (PInt val)

style :: (Sel2 a) => Text -> Text -> Chain a a
style key val = func' "style" [PText key, PText val]

-- | classed(). Take a list of classes as an argument.
classed :: (Sel2 a) => [Text] -> Chain a a
classed kls = func' "classed" [PText (T.intercalate " " kls)]

property :: (Sel2 a) => Text -> Text -> Chain a a
property key val = func' "property" [PText key, PText val]

text :: (Sel2 a) => Text -> Chain a a
text val = func' "text" [PText val]

html :: (Sel2 a) => Text -> Chain a a
html val = func' "html" [PText val]

width :: (Sel2 a) => Double -> Chain a a
width v = attr "width" (PDouble v)

height :: (Sel2 a) => Double -> Chain a a
height v = attr "height" (PDouble v)

transform :: (Sel2 a) => Text -> Chain a a
transform = attrt "transform"

transform' :: (Sel2 a) => Double -> Double -> Double -> Double -> Double -> Chain a a
transform' tx ty sx sy r =
	attrt "transform" $
		T.concat ["translate(",f tx, " ",f ty,") scale(",f sx, " ", f sy, ") rotate(",f r, ")"]
	where
		f v = T.pack $ show v

opacity :: Sel2 a => Double -> Chain a a
opacity = attrd "fill-opacity"

fill :: Sel2 a => JSParam -> Chain a a
fill p = func "style" [PText "fill", p]

fill' :: Sel2 a => Text -> Chain a a
fill' = style "fill"

-- * Color
hsl :: JSParam -> JSParam -> JSParam -> NumFunc Color
hsl h s l = ApplyFunc' "d3.hsl" [h,s,l]

-- * Animation and Interaction

-- | on()
on :: (JSObjClass a) => Text -> FuncDef -> Chain a a
on typ f = func "on" [PText typ,PFunc f]

mouse :: Text -> FuncDef
mouse container = FuncTxt $ T.concat ["d3.mouse(",container,")"]

touches :: Text -> FuncDef
touches container = FuncTxt $ T.concat ["d3.touches(",container,")"]

-- | transition()
transition :: (Sel2 a) => Chain a Transition
transition = func "transition" []

-- | trasition().duration(time). time can be any of "JSParam"
transition' :: (Sel2 a) => JSParam -> Chain a Transition
transition' d = transition >>> func "duration" [d]

-- | trasition().duration(time). time can be any of "JSParam"
transition_d :: (Sel2 a) => Double -> Chain a Transition
transition_d d = transition >>> funcd1 "duration" d

interrupt :: (JSObjClass a) => Chain a a
interrupt = func "interrupt" []

-- | delay()
delay :: JSParam -> Chain Transition Transition
delay v = func "delay" [v]

-- * Control

-- | each()
each :: (JSObjClass a) => FuncDef -> Chain a a
each f = func "each" [PFunc f]

-- | call()
call :: (JSObjClass a) => FuncDef -> Chain a a
call f = func "call" [PFunc f]

-- | empty()
empty :: (Sel a) => Chain a Bool
empty = func "empty" []

-- | node()
node :: (Sel a) => Chain a a
node = func "node" []

-- | size()
size :: (Sel a) => Chain a Int
size = func "size" []

-- | size by attributes width, height
size_ w h = attrd "width" w >>> attrd "height" h
sizei_ w h = attrd "width" (fromIntegral w) >>> attrd "height" (fromIntegral h)

-- * Transitions

-- * Arrays
range :: Int -> Chain () JSObjArray
range to = d3Root >>> funci1 "range" to


-- * Scales

scale :: ChainValue Scale
scale = Val "d3.scale"

category10 :: Chain () Scale
category10 = scale >>> func "category10" []

linear :: JSParam -> JSParam -> JSParam -> JSParam -> Chain () Scale
linear a b c d =
	(func "range" [PArray [c, d]] :: IsoChain Scale) .
	(func "domain" [PArray [a, b]] :: IsoChain Scale) .
	(func "linear" [] :: IsoChain Scale) .
	Val "d3.scale"

ticks :: Int -> Var' Scale -> Chain () JSObjArray
ticks n (Var' name) = Val name >>> (funci1 "ticks" n :: Chain Scale JSObjArray)

-- * String formatting
format :: Text -> Chain () D3Func
format t = funct1 "format" t . d3Root

-- * Layout

layout :: Chain () JSObj
layout = Val "d3.layout"

histogram :: Chain () Histogram
histogram = funct0 "histogram" . layout

bins :: Int -> Chain Histogram (JSFunc JSObjArray b)
bins n = funci1 "bins" n

calcHist :: Var' a -> ChainValue (JSFunc JSObjArray JSObjArray) -> ChainValue JSObjArray
calcHist (Var' v) histbinfunc = apply [ParamVar v] histbinfunc

apply :: [JSParam] -> Chain a (JSFunc params r) -> Chain a r
apply = Apply 

-- * Force

force :: Chain () Force
force = funct0 "force" . layout

gravity :: Double -> Chain Force Force
gravity = funcd1 "gravity"

charge :: JSParam -> Chain Force Force
charge v = func "charge" [v]

nodes :: Var' r -> Chain Force Force
nodes (Var' d) = func "nodes" [ParamVar d]

force_size :: (Double,Double) -> Chain Force Force
force_size (w,h) = func "size" [PArray [(PDouble w),(PDouble h)]]
-- * Helper functions for Chain a b type

-- |Apply a function
func :: (JSObjClass a) => FuncName -> [JSParam] -> Chain a b
func name params = Apply params $ Refine name

funct1 :: (JSObjClass a) => FuncName -> Text -> Chain a b
funct1 name t = func name [PText t]

funct0 :: (JSObjClass a) => Text -> Chain a b
funct0 name = func name []

funci1 name v = func name [PInt v]

funcd1 name v = func name [PDouble v]

field :: Text -> Chain JSObj b
field n = Refine n

-- |Function that does not change type in a method chain.
func' :: (JSObjClass a) => FuncName -> [JSParam] -> Chain a a
func' = func


-- * Math

bates :: Int -> NumFunc JSObjArray
bates n = ApplyFunc' "d3.random.bates" [PInt n]
