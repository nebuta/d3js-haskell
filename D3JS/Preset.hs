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

enterData :: Sel2 a => SvgElement -> Var' JSObjArray -> Chain a (SelData JSObjArray)
enterData elem dat = selectAll (T.pack $ show elem) >>> dataD3 dat >>> enter >>> appendD3 (T.pack $ show elem)

mkSvg :: Text -> (Int,Int) -> St (Var' Selection)
mkSvg elem (w,h) = assign $ d3Root >>> select elem >>> appendD3 "svg" >>> sizei_ w h
