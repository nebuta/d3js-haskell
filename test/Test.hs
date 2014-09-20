-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}

{-# LANGUAGE OverloadedStrings,NoImplicitPrelude #-}

module Example where

import D3JS
import Test.Hspec
import Control.Monad.IO.Class
import Control.Monad
import System.Random
import Data.Text (Text)
import qualified Data.Text as T 
import qualified Data.Text.IO as T

import qualified Data.Map as M
import Data.Map (Map)

import D3JS.Example 

import Prelude hiding (id,(.))
import Control.Category
import Control.Applicative

rects :: St ()
rects = do
	dat <- assign $ range 10 >>> mapD3 ((mkObj [("v",random_*10)]))
	svg <- assign $ mkSvg "body" 400 400
	execute $ svg .> enterData Rect dat
		>>> sizei_ 40 40
		>>> attrf "x" (i_ * 40)
		>>> attrd "y" 0
		>>> fill (funcExp $ hsl (PFunc' $ (d_ ..> "v" * (NDouble 360))) 0.5 0.5)

rects2d :: St ()
rects2d = do
	dat <- assign $ range 10 >>> mapD3 (ChainVal $ mapD3' "function(dd,j){return {x: i, y: j, v: Math.random()};}" . range 10)
	svg <- assign $ mkSvg "body" 400 400
	cols <- assign $ svg .> enterData G (dat :: Var' JSObjArray)
	execute $ cols .> enterData' Rect objConst >>> sizei_ 20 20
		>>> xyset
		>>> fill (funcExp $ hsl (PFunc' $ (d_ ..> "v" * (NDouble 360))) 0.5 0.5)
	return ()

rects' = do
	let dat = Data2D [(1,2),(23,3)]
	mapToD3 "body" 400 400 dat (M.fromList [])

-- Draw rects
mapToD3 :: Text -> Int -> Int -> Data2D -> Map Text (NumFunc r) -> St ()
mapToD3 parent width height dat fs = do
	svg <- assign $ mkSvg parent (PInt width) (PInt height)
	return () -- stub

-- http://bl.ocks.org/mbostock/3048450
histogram1 :: St ()
histogram1 = do
	values <- assign $ range 1000 >>> mapD3' (reify (bates 10)) :: St (Var' JSObjArray)
	formatCount <- assign $ format ",.0f"
	let (w,h) = (800,400)
	x <- assign $ linear 0 1 0 w
	dat@(Var' dat') <- assign $ calcHist values $ bins 10 . histogram
	y <- assign $ linear 0 (PChainValue $ maxD3 dat (FuncExp y_)) h 0
	svg <- assign $ mkSvg "body" w h >>> appendD3 G >>> transform' 10 10 0 0 0
	bar <- assign $ svg .> selectAll ".bar" >>> dataD3 dat >>> enter >>> appendD3 G >>> attrt "class" "bar"
		>>> transform "function(d) { return 'translate(' + x(d.x) + ','' + y(d.y) + ')'; }"
	execute $ bar .> appendD3 Rect >>> attr "x" 1 >>> attr "width" 300
			 
	return ()


xyset = attrf "x" (x_ * 20) . attrf "y" (y_ * 20)

tests = [
		("rects_2.html",rects')
		,("rects2d.html",rects2d)
		,("histogram.html",histogram1)]

objConst = DataParam :: NumFunc JSObjArray

runAll = forM_ tests $ \(n,st) -> writeToHtml n st

rand2D :: Int -> IO [(Double,Double)]
rand2D n = do
	xs <- replicateM n $ getStdRandom (randomR (1,100))
	ys <- replicateM n $ getStdRandom (randomR (1,100))
	return (zip xs ys)

d3jsUrl = "http://d3js.org/d3.v3.min.js"
