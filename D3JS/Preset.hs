{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}

-- |This module has functions that are common patterns. They are just compositions of primitives.
module D3JS.Preset where

import Prelude hiding ((.),id)
import Control.Category

import Data.Text (Text)
import qualified Data.Text as T

import D3JS.Type
import D3JS.Func
import D3JS.Syntax

-- * Accessors

-- | d[0] as a user-defined function.
idx0,idx1 :: NumFunc Double
idx0 = Index (NInt 0) DataParam

-- | d[1] as a user-defined function.
idx1 = Index (NInt 1) DataParam

-- | d.x as a user-defined function.
x_ :: NumFunc Double
x_ = DataParam ..> "x"

-- | d.y as a user-defined function.
y_ :: NumFunc Double
y_ = DataParam ..> "y"

i_ :: NumFunc Int
i_ = DataIndex

d_ :: NumFunc r
d_ = DataParam

-- * Method chains

enterData :: (Sel2 a, DataArg b) => SvgElement -> Var' b -> Chain a (SelData b)
enterData elem dat = selectAll (T.pack $ show elem) >>> dataD3 dat >>> enter >>> appendD3 elem

enterData' :: (Sel2 a, DataArg b) => SvgElement -> NumFunc b -> Chain a (SelData b)
enterData' elem dat = selectAll (T.pack $ show elem) >>> dataD3' dat >>> enter >>> appendD3 elem

mkSvg :: Text -> JSParam -> JSParam -> ChainValue Selection
mkSvg parent w h = d3Root >>> select parent >>> appendD3 Svg >>> attr "width" w >>> attr "height" h
