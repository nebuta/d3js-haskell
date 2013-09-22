{-# LANGUAGE OverloadedStrings,MultiParamTypeClasses,NoImplicitPrelude #-}

-- |This modules provides high-level functions for drawing common charts, such as bar charts and scatter plots.
--  Those functions also exemplify how to compose primitive functions to achieve complex drawing.
--  This module will be expanded in the near future.
module D3JS.Chart where

import D3JS.Type
import D3JS.Func
import D3JS.Syntax
import D3JS.Reify

import Prelude hiding ((.),id)
import Control.Category
import Data.Text (Text)
import qualified Data.Text as T

-- | box parent (w,h) makes an SVG container in a parent element with dimension w x h.
box :: Selector ->  (Double,Double) -> St (Var' Selection)
box parent (w,h) = do
	assign
		$ ((d3Root
			>>> select parent
			>>> func "append" [PText "svg"]
			>>> width w
			>>> height h
			>>> style "background" "#eef") :: Chain () Selection)

bars :: Int -> Double -> Data1D -> Var' Selection -> St ()
bars n width ps (Var' elem) = do
	let bar_w = width / (fromIntegral n)
	v <- assign $ Val' (mkRectData bar_w ps)
	execute $
		(Val elem :: Chain () Selection)
		>>> addRect v
		>>> fill "red"

scatter :: Data2D -> Var' Selection -> St (Var' (SelData Data2D))
scatter ps (Var' elem) = do
	v <- assign $ Val' ps
	cs <- assign $
		(Val elem :: Chain () Selection)
		>>> addCircles v
	return cs

-- | Add rectangles with an array of objects {x: x, y: y, width: w , height: h}
addRect :: Sel2 a => Var' RectData -> Chain a (SelData RectData)
addRect dat =
	selectAll "rect" >>> dataD3 dat >>> enter >>> appendD3 "rect"
	>>> attr "x" (funcExp _x)
	>>> attr "y" (funcExp _y)
	>>> attr "width" (funcExp $ Field "width" DataParam)
	>>> attr "height" (funcExp $ Field "height" DataParam)

mkRectData :: Double -> Data1D -> RectData
mkRectData bar_width (Data1D ps) =
	RectData $ flip map (zip ps [0..])$ \(v,i) ->
		(bar_width*i,300-v,bar_width*0.95,v)


addCircles :: Sel2 a => Var' Data2D -> Chain a (SelData Data2D)
addCircles dat = 
	selectAll "circle"
		>>> dataD3 dat
		>>> enter
		>>> appendD3 "circle"
		>>> attrt "class" "p"
		>>> attrd "r" 3
		>>> fill "blue"
		>>> attr "cx" (funcExp idx0)
		>>> attr "cy" (funcExp idx1)

-- | disappear delay duration
disappear :: (Sel2 a) => Double -> Double -> Var' a -> St ()
disappear delay_ duration var = do
	execute $
		Val'' var
		>>> transition' duration
		>>> attrd "r" 10
		>>> delay (PDouble delay_)
		>>> style "opacity" "0"

addFrame :: Sel2 a => (Double,Double) -> (Double,Double) -> Var' a -> St ()
addFrame (w,h) (w2,h2) box = do
	let dx = (w-w2)/2
	let dy = (h-h2)/2
	let sx = w2/w
	let sy = h2/h
	execute $
		Val'' box
		>>> selectAll ".p"  -- means data points.
		>>> transform' dx dy sx sy 0
	v <- assign $ Val' $ RectData [(dx,dy,w2,h2)]
	execute $
		Val'' box
		>>> addRect v
		>>> fill "none"
		>>> attrt "stroke" "black"
		>>> attrd "stroke-width" 1

data RectData = RectData [(Double,Double,Double,Double)] -- x,y,width,height
instance Reifiable RectData where
	reify (RectData vs) = surround $ T.intercalate "," $
		flip map vs $
			(\(x,y,w,h) -> T.concat ["{x:",show' x,",y:",show' y,",width:",show' w,",height:",show' h,"}"])

instance Assignable RectData where
	newVar = newVar' "dat"


