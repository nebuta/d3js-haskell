{-# LANGUAGE OverloadedStrings #-}

-- |This module defines original functions of D3.js, as well as some low-level helper functions.
module D3JS.Func where

import D3JS.Type

import Data.List
import Data.Text (Text)
import qualified Data.Text as T 

import Prelude hiding ((.),id)
import Control.Category

-- * Selection

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

-- | enter()
enter :: Chain (SelData r) (SelData r)
enter = func "enter" []

-- | exit()
exit :: (Sel a) => Chain a a
exit = func "exit" []

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

opacity :: Sel a => Double -> Chain a a
opacity = attrd "fill-opacity"

fill :: Sel a => Text -> Chain a a
fill = style "fill"

-- * Transitions

-- | transition()
transition :: Chain Selection Transition
transition = func "transition" []

-- | trasition().delay(time)
transition' :: Double -> Chain Selection Transition
transition' d = transition >>> funcd1 "duration" d

-- | delay()
delay :: JSParam -> Chain Transition Transition
delay v = func "delay" [v]

-- * Helper functions for Chain a b type

func :: FuncName -> [JSParam] -> Chain a b
func name params = Func $ JSFunc name params

funct1 name t = func name [PText t]

funcd1 name v = func name [PDouble v]

-- |Function that does not change type in a method chain.
func' :: FuncName -> [JSParam] -> Chain a a
func' = func

funcTxt :: Text -> JSParam
funcTxt = PFunc . FuncTxt

funcExp = PFunc . FuncExp
