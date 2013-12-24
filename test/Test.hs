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

import D3JS.Example 

import Prelude hiding (id,(.))
import Control.Category

rects :: St ()
rects = do
		dat <- assign $ range 10 >>> mapD3 ((MkObject [("v",random_*10)]))
		svg <- mkSvg "body" (400,400)
		execute $ svg .> enterData Rect dat
			>>> sizei_ 40 40
			>>> attrf "x" (DataIndexD * 40.8)
			>>> attrd "y" 0
			>>> fill (funcExp $ hsl (PFunc' $ (DataParam ..> "v" * (NDouble 360))) 0.5 0.5)
		return ()

rects2d :: St ()
rects2d = do
		dat <- assign $ range 10 >>> mapD3 ((MkObject [("v",random_*10)]))
		svg <- mkSvg "body" (400,400)
		execute $ svg .> enterData Rect dat
			>>> sizei_ 40 40
			>>> attrf "x" (DataIndexD * 40.8)
			>>> attrd "y" 0
			>>> fill (funcExp $ hsl (PFunc' $ (DataParam ..> "v" * (NDouble 360))) 0.5 0.5)
		return ()

tests = [("rects2d.html",rects2d)]

runAll = forM_ tests $ \(n,st) -> writeToHtml n st

rand2D :: Int -> IO [(Double,Double)]
rand2D n = do
	xs <- replicateM n $ getStdRandom (randomR (1,100))
	ys <- replicateM n $ getStdRandom (randomR (1,100))
	return (zip xs ys)

d3jsUrl = "http://d3js.org/d3.v3.min.js"
