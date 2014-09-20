{-# LANGUAGE OverloadedStrings,NoImplicitPrelude #-}

module D3JS.Example (
	test1
	, test2
	, test3
	, collision
	, writeToHtml
) where

import D3JS

import Data.Text (Text)
import qualified Data.Text as T 
import qualified Data.Text.IO as T
import Control.Monad
import System.Random

import Prelude hiding (id,(.))
import Control.Category

-- * Examples

test1, test2, test3 :: IO ()
-- | Scatter plot with a frame. Generate 'test1.html' file.
test1 = do
	ps <- rand2D 100
	writeToHtml "test1.html" (graph1 ps)

-- | Scatter plot with dissolving transition. Generate 'test2.html' file.
test2 = do
	ps <- rand2D 100
	writeToHtml "test2.html" (graph2 ps)

-- | Bar chart. Generate 'test2.html' file.
test3 = do
	ps <- replicateM 10 $ getStdRandom (randomR (100,300))
	writeToHtml "test3.html" (graph3 ps)

-- | http://mbostock.github.io/d3/talk/20111018/collision.html
collision = do
	writeToHtml "collision.html" collision'

graph1,graph2 :: [(Double,Double)] -> St ()
graph3 :: [Double] -> St ()

graph1 ps = do
	let dim = (300,300)
	elem <- box "#div1" dim
	scatter (mkScatter (Data2D ps)) elem
	addFrame (Size 300 300) (Size 250 250) elem 

graph2 ps = box "#div1" (300,300) >>= scatter (mkScatter (Data2D ps)) >>= disappear 500 1000

graph3 ps = do
	elem <- box "#div1" (300,300)
	bars 10 300 (Data1D ps) elem

rand2D :: Int -> IO [(Double,Double)]
rand2D n = do
	xs <- replicateM n $ getStdRandom (randomR (1,100))
	ys <- replicateM n $ getStdRandom (randomR (1,100))
	return (zip xs ys)

-- | Output a single excutable HTML file with embedded JS code.
writeToHtml :: (Reifiable a) => FilePath -> a -> IO ()
writeToHtml path a = T.writeFile path $ T.concat ["<html> <head><body> <div id='body'></div> <script src='",d3jsUrl,"' charset='utf-8'></script> <script charset='utf-8'>\n",reify a,"\n</script> </body> </html>"]


d3jsUrl = "./d3.js"
-- d3jsUrl = "http://d3js.org/d3.v3.min.js"


collision' :: St ()
collision' = do
	let (w,h) = (1280,800)
	ns <- assign $ range 200 >>> mapD3 (MkObject [("radius",random_ * 12 + 4)]) :: St (Var' JSObjArray)
	color <- assign $ category10
	force' <- assign $
				force
				>>> gravity 0.05
				>>> charge (funcExp $ Ternary DataIndex 0 (NInt (-2000)))
				>>> nodes ns
				>>> force_size (w,h)
	execute $ Val' force' >>> func "start" []
	svg <- assign $ d3Root
			>>> select "#body"
			>>> appendD3 Svg
			>>> size_ w h
	d <- assign $ Val' ns >>> funci1 "slice" 1
	execute $
		Val' svg >>> selectAll "circle"
			>>> dataD3 (d :: Var' Data1D)
			>>> enter
			>>> appendD3 Circle
			>>> attr "r" (funcExp (Field "radius" DataParam - (NDouble 2)))
			>>> fill (funcExp (ApplyFunc color [funcExp (DataIndex % (NInt 3))]))
	execute $ Val' force' >>> on "tick" (FuncTxt $ "function(e){}")	
	return ()






















