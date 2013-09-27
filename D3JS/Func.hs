{-# LANGUAGE OverloadedStrings #-}

-- |This module defines original functions of D3.js, as well as some low-level helper functions.
module D3JS.Func where

import D3JS.Type

import Data.List
import Data.Text (Text)
import qualified Data.Text as T 

import Prelude hiding ((.),id)
import Control.Category

-- * Selection and data assignment

-- | d3 object in D3.js
d3Root :: Chain () Selection
d3Root = Val "d3"

-- | select() in D3.js
select :: Selector -> Chain Selection Selection
select = funct1 "select"

-- | selectAll()
selectAll :: Sel2 a => Selector -> Chain a Selection
selectAll = funct1 "selectAll"

-- * Data manipulation

-- | data() in D3.js. Assigns new data to selection.
dataD3 :: Var' r -> Chain Selection (SelData r)
dataD3 (Var' d) = func "data" [ParamVar d]

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
appendD3 :: Text -> Chain (SelData a) (SelData a)
appendD3 = funct1 "append"

-- * Attributes and styles

attr :: Text -> JSParam -> Chain a a
attr key val = func' "attr" [PText key, val]

attrf :: Text -> JSParam -> Chain a a
attrf key val = func' "attr" [PText key, val]

attrt :: Text -> Text -> Chain a a
attrt key val = attr key (PText val)

attrd :: Text -> Double -> Chain a a
attrd key val = attr key (PDouble val)

style :: Text -> Text -> Chain a a
style key val = func' "style" [PText key, PText val]

-- | classed(). Take a list of classes as an argument.
classed :: [Text] -> Chain a a
classed kls = func' "classed" [PText (T.intercalate " " kls)]

property :: Text -> Text -> Chain a a
property key val = func' "property" [PText key, PText val]

text :: Text -> Chain a a
text val = func' "text" [PText val]

html :: Text -> Chain a a
html val = func' "html" [PText val]

width :: Double -> Chain a a
width v = attr "width" (PDouble v)

height :: Double -> Chain a a
height v = attr "height" (PDouble v)

transform = attrt "transform"

transform' :: Double -> Double -> Double -> Double -> Double -> Chain a a
transform' tx ty sx sy r =
	attrt "transform" $
		T.concat ["translate(",f tx, " ",f ty,") scale(",f sx, " ", f sy, ") rotate(",f r, ")"]
	where
		f v = T.pack $ show v

opacity :: Sel a => Double -> Chain a a
opacity = attrd "fill-opacity"

fill :: Sel a => Text -> Chain a a
fill = style "fill"

-- * Animation and Interaction

-- | on()
on :: Text -> FuncDef -> Chain (SelData r) (SelData r)
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

interrupt = func "interrupt" []

-- | delay()
delay :: JSParam -> Chain Transition Transition
delay v = func "delay" [v]

-- * Control

-- | each()
each :: FuncDef -> Chain a a
each f = func "each" [PFunc f]

-- | call()
call :: FuncDef -> Chain a a
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


-- * Transitions


-- * Helper functions for Chain a b type

func :: FuncName -> [JSParam] -> Chain a b
func name params = Func $ JSFunc name params

funct1 name t = func name [PText t]

funcd1 name v = func name [PDouble v]

-- |Function that does not change type in a method chain.
func' :: FuncName -> [JSParam] -> Chain a a
func' = func


