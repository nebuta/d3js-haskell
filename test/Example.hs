{-# OPTIONS_GHC -F -pgmF hspec-discover #-}

module Example where


test1 :: IO ()
-- | Scatter plot with a frame. Generate 'test1.html' file.
test1 = do
	ps <- rand2D 100
	writeToHtml "test1.html" (graph1 ps)

graph1 :: [(Double,Double)] -> St ()
graph1 ps = do
	let dim = (300,300)
	elem <- box "#div1" dim
	scatter (mkScatter (Data2D ps)) elem
	addFrame (Size 300 300) (Size 250 250) elem 