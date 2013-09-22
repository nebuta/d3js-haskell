{-# LANGUAGE OverloadedStrings #-}

module D3JS.Example (
	test
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

-- | Scatter plot. Generate 'generated.js' file.
test :: IO ()
test = do
	ps <- fmap Data2D $ randPs 100
	let graph = reify $ do
		elem <- box "#div1" (300,300)
		e <- scatter ps elem
		addFrame e
	T.writeFile "generated.js" graph

-- | Scatter plot with smaller size. Generate 'generated.js' file.
test2 :: IO ()
test2 = do
	ps <- fmap Data2D $ randPs 100
	let graph = reify $ (box "#div1" (300,300) >>= scatter ps >>= disappear 500 1000)
	T.writeFile "generated.js" graph

-- | Scatter plot with dissolving transition. Generate 'generated.js' file.
test3 :: Int -> IO ()
test3 n = do
	vs <- replicateM n $ getStdRandom (randomR (100,300))
	T.writeFile "generated.js" $ reify $ do
		elem <- box "#div1" (300,300)
		bars n 300 (Data1D vs) elem

randPs :: Int -> IO [(Double,Double)]
randPs n = do
	xs <- replicateM n $ getStdRandom (randomR (1,300))
	ys <- replicateM n $ getStdRandom (randomR (1,300))
	return (zip xs ys)

-- | Output a single excutable HTML file with embedded JS code.
writeToHtml :: (Reifiable a) => FilePath -> a -> IO ()
writeToHtml path a = T.writeFile path $ T.concat ["<html> <head><body> <div id='div1'></div> <script src='http://d3js.org/d3.v3.min.js' charset='utf-8'></script> <script charset='utf-8'>\n",reify a,"\n</script> </body> </html>"]

