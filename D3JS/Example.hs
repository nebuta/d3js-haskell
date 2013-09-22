{-# LANGUAGE OverloadedStrings #-}

module D3JS.Example (
	test1
	, test2
	, test3
	, writeToHtml
) where

import D3JS

import Data.Text (Text)
import qualified Data.Text as T 
import qualified Data.Text.IO as T
import Control.Monad
import System.Random

-- * Examples

-- | Scatter plot with a frame. Generate 'test1.html' file.
test1, test2, test3 :: IO ()
test1 = do
	ps <- rand2D 100
	writeToHtml "test1.html" (graph1 ps)

-- | Scatter plot with dissolving transition. Generate 'generated.js' file.
test2 = do
	ps <- rand2D 100
	writeToHtml "test2.html" (graph2 ps)

-- | Bar chart. Generate 'generated.js' file.
test3 = do
	ps <- replicateM 10 $ getStdRandom (randomR (100,300))
	writeToHtml "test3.html" (graph3 ps)

graph1,graph2 :: [(Double,Double)] -> St ()
graph3 :: [Double] -> St ()

graph1 ps = do
	let dim = (300,300)
	elem <- box "#div1" dim
	scatter (Data2D ps) elem
	addFrame dim (250,250) elem 

graph2 ps = box "#div1" (300,300) >>= scatter (Data2D ps) >>= disappear 500 1000

graph3 ps = do
	elem <- box "#div1" (300,300)
	bars 10 300 (Data1D ps) elem

rand2D :: Int -> IO [(Double,Double)]
rand2D n = do
	xs <- replicateM n $ getStdRandom (randomR (1,300))
	ys <- replicateM n $ getStdRandom (randomR (1,300))
	return (zip xs ys)

-- | Output a single excutable HTML file with embedded JS code.
writeToHtml :: (Reifiable a) => FilePath -> a -> IO ()
writeToHtml path a = T.writeFile path $ T.concat ["<html> <head><body> <div id='div1'></div> <script src='http://d3js.org/d3.v3.min.js' charset='utf-8'></script> <script charset='utf-8'>\n",reify a,"\n</script> </body> </html>"]

