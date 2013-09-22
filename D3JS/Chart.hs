{-# LANGUAGE OverloadedStrings,MultiParamTypeClasses #-}

-- |This modules provides high-level functions for drawing common charts, such as bar charts and scatter plots.
--  Those functions also exemplify how to compose primitive functions to achieve complex drawing.
--  This module will be expanded in the near future.
module D3JS.Chart where

import D3JS.Type
import D3JS.Func
import D3JS.Syntax
import D3JS.Reify

import Control.Category
import Data.Text (Text)
import qualified Data.Text as T

-- | box parent (w,h) makes an SVG container in a parent element with dimension w x h.
box :: Selector ->  (Int,Int) -> St (Var' Selection)
box parent (w,h) = do
	assign
		$ ((d3Root
			>>> select parent
			>>> func "append" [PText "svg"]
			>>> width (fromIntegral w)
			>>> height (fromIntegral h)
			>>> style "background" "#eef") :: Chain () Selection)

bars :: Int -> Double -> Data1D -> Var' Selection -> St ()
bars n width ps (Var' elem) = do
	let bar_w = width / (fromIntegral n)
	v <- assign $ Val' (mkRectData bar_w ps)
	execute $
		(Val elem :: Chain () Selection)
		>>> addRect v

scatter :: Data2D -> Var' Selection -> St (Var' Selection)
scatter ps (Var' elem) = do
	v <- assign $ Val' ps
	execute $
		(Val elem :: Chain () Selection)
		>>> addCircles v
	return (Var' elem)

-- | Add rectangles with an array of objects {x: x, y: y, width: w , height: h}
addRect :: Sel2 a => Var' RectData -> Chain a (SelData RectData)
addRect dat =
	selectAll "rect" >>> dataD3 dat >>> enter >>> appendD3 "rect"
	>>> attr "x" (funcExp _x)
	>>> attr "y" (funcExp _y)
	>>> attr "width" (funcExp $ Field "width" DataParam)
	>>> attr "height" (funcExp $ Field "height" DataParam)
	>>> fill "red"

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
		>>> attrd "r" 3
		>>> fill "blue"
		>>> attr "cx" (funcExp idx0)
		>>> attr "cy" (funcExp idx1)
	--	>>> attr "transform" (PText "translate(100)")

-- | disappear delay duration
disappear :: Double -> Double -> Var' Selection -> St ()
disappear delay_ duration var = do
	execute $
		Val'' var
		>>> selectAll "circle"
		>>> transition' duration
		>>> attrd "r" 10
		>>> delay (PDouble delay_)
--			>>> delay (funcDef "function(d, i) { return i * 10; }")
		>>> style "opacity" "0"


addFrame :: Var' Selection -> St ()
addFrame (Var' elem) = do
	execute $
		Val' elem
		>>> transform "translate(50 50) scale(0.67 0.67)"

data RectData = RectData [(Double,Double,Double,Double)] -- x,y,width,height
instance Reifiable RectData where
	reify (RectData vs) = surround $ T.intercalate "," $
		flip map vs $
			(\(x,y,w,h) -> T.concat ["{x:",show' x,",y:",show' y,",width:",show' w,",height:",show' h,"}"])

instance Assignable RectData where
	newVar = newVar' "dat"


