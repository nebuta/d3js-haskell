{-# LANGUAGE OverloadedStrings,MultiParamTypeClasses,NoImplicitPrelude,GeneralizedNewtypeDeriving,FlexibleInstances #-}

-- |This modules provides high-level functions for drawing common charts, such as bar charts and scatter plots.
--  Those functions also exemplify how to compose primitive functions to achieve complex drawing.
--  This module will be expanded in the near future.
module D3JS.Chart where

import D3JS.Type
import D3JS.Func
import D3JS.Syntax
import D3JS.Reify
import D3JS.Preset

import Prelude hiding ((.),id)
import Control.Category
import Data.Text (Text)
import qualified Data.Text as T

-- import Debug.Trace

-- import Data.Default

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
		>>> fill' "red"

-- ticks :: [(Double,Double)] -> Var' Selection -> St ()
-- ticks vs (Var' elem) = undefined

data Range a = Range a a deriving (Show)
newtype Scalar = Scalar {unScalar :: Double} deriving (Show,Num,Real,Eq,Ord,Fractional)
newtype Coord1 = Coord1 {unCoord1 :: Double} deriving (Show,Num,Real,Eq,Ord,Fractional)  -- Used for length and coord in actual drawing in svg.
data Ticks = Ticks [(Scalar,Coord1)] deriving (Show)

data Scatter = Scatter (Range Coord1) (Range Coord1) Ticks Ticks Data2D deriving (Show)

autoTick :: Range Coord1 -> Range Scalar -> Ticks
autoTick cr@(Range cmin cmax) vr@(Range vmin vmax) =
	let
		n = 5  -- stub
		vint = (vmax-vmin)/Scalar n -- stub
		vs = map (\i -> vmin + vint * Scalar i) [0..n]
		cs = map (scaleInRange cr vr) vs -- map (\i -> cmin + cint * Coord1 i) [0..n]
	in Ticks (zipWith (\v c -> (v, c)) vs cs)

-- make scatter with auto range.
mkScatter :: Data2D -> Scatter
mkScatter ps@(Data2D vs) =
	let
		xs = map fst vs
		ys = map snd vs
		cx = Range 0 300
		cy = Range 0 300
		tx = autoTick cx $ Range (Scalar $ minimum xs) (Scalar $ maximum xs)
		ty = autoTick cy $ Range (Scalar $ minimum ys) (Scalar $ maximum ys)
	in Scatter cx cy tx ty ps

scaleInRange :: Range Coord1 -> Range Scalar -> (Scalar -> Coord1)
scaleInRange (Range cmin cmax) (Range vmin vmax) v =
	cmin + (Coord1 . unScalar)((v-vmin)/(vmax-vmin) * (Scalar . unCoord1) (cmax-cmin))

scatter :: Scatter -> Var' Selection -> St (Var' (SelData Data2D))
scatter s@(Scatter rx ry tx ty ps) (Var' elem) = do
	v <- assign $ Val' ps
	cs <- assign $
		(Val elem :: Chain () Selection)
		>>> addCircles v
	return cs

-- | Add rectangles with an array of objects {x: x, y: y, width: w , height: h}
addRect :: Sel2 a => Var' RectData -> Chain a (SelData RectData)
addRect dat =
	enterData Rect dat
	>>> attr "x" (funcExp x_)
	>>> attr "y" (funcExp y_)
	>>> attr "width" (funcExp $ Field "width" DataParam)
	>>> attr "height" (funcExp $ Field "height" DataParam)

instance DataArg RectData
instance DataArg Data2D

mkRectData :: Double -> Data1D -> RectData
mkRectData bar_width (Data1D ps) =
	RectData $ flip map (zip ps [0..])$ \(v,i) ->
		(Scalar (bar_width*i),Scalar (300-v),Scalar bar_width*0.95,Scalar v)


addCircles :: Sel2 a => Var' Data2D -> Chain a (SelData Data2D)
addCircles dat = 
	selectAll "circle"
		>>> dataD3 dat
		>>> enter
		>>> appendD3 Circle
		>>> attrt "class" "p"
		>>> attrd "r" 3
		>>> fill' "blue"
		>>> attr "cx" (funcExp idx0)
		>>> attr "cy" (funcExp idx1)

-- | disappear delay duration
disappear :: (Sel2 a) => Double -> Double -> Var' a -> St ()
disappear delay_ duration var = do
	execute $
		Val'' var
		>>> transition' (funcExp (100 * idx0))
		>>> attrd "r" 10
		>>> delay (PDouble delay_)
		>>> style "opacity" "0"

addFrame :: Sel2 a => Size -> Size -> Var' a -> St ()
addFrame (Size w h) (Size w2 h2) box = do
	let dx = (w-w2)/2
	let dy = (h-h2)/2
	let sx = w2/w
	let sy = h2/h
	execute $
		Val'' box
		>>> selectAll ".p"  -- means data points.
		>>> transform' dx dy sx sy 0
	v <- assign $ Val' $ RectData [(Scalar dx,Scalar dy,Scalar w2,Scalar h2)]
	execute $
		Val'' box
		>>> addRect v
		>>> fill' "none"
		>>> attrt "stroke" "black"
		>>> attrd "stroke-width" 1

data Size = Size Double Double -- width, height

data RectCoord = RectCoord Double Double Double Double

toCoord :: Scalar -> Coord1
toCoord = Coord1 . unScalar

data RectData = RectData [(Scalar,Scalar,Scalar,Scalar)] -- x,y,width,height
  deriving (Show)

instance Reifiable RectData where
	reify (RectData vs) = surround $ T.intercalate "," $
		flip map vs $
			(\(Scalar x,Scalar y,Scalar w,Scalar h) -> T.concat ["{x:",show' x,",y:",show' y,",width:",show' w,",height:",show' h,"}"])

instance Assignable RectData where
	newVar = newVar' "dat"


