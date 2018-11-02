module Shapes where

import Graphics.UI.GLUT

data Vec = Vec Float Float deriving Show
vertexVec :: Vec -> IO ()
vertexVec (Vec x y) = vertex $ Vertex2 x y
add :: Vec -> Vec -> Vec
(Vec x0 y0) `add` (Vec x y) = Vec (x0 + x) (y0 + y)
sub :: Vec -> Vec -> Vec
(Vec x0 y0) `sub` (Vec x y) = Vec (x0 - x) (y0 - y)
mul :: Vec -> Float -> Vec
(Vec x y) `mul` a = Vec (a * x) (a * y)
equals :: Vec -> Vec -> Bool
(Vec x0 y0) `equals` (Vec x y) = x0 == x && y0 == y
dist :: Vec -> Vec -> Float
dist (Vec x0 y0) (Vec x1 y1) = sqrt $ (x1 - x0) ^ 2 + (y1 -y0) ^ 2

vecToAngle :: Vec -> Float
vecToAngle (Vec x y) = atan (y / x) + addition
	where addition = if x < 0 then pi else if y < 0 then 2 * pi else 0

angleToVec :: Vec -> Float -> Float -> Vec
angleToVec (Vec x0 y0) radius angle =
	Vec (x0 + radius * (cos angle)) (y0 + radius * (sin angle))

data Col = Col Float Float Float deriving Show
setColor :: Col -> IO ()
setColor (Col r g b) = do
	color $ Color3 r g (b :: GLfloat)

data Poly = Poly Vec [Vec] Col
drawPoly :: Poly -> IO ()
drawPoly (Poly p points col) = do
	drawPrimitive Polygon points col

circle :: Vec -> Float -> Col -> Int -> Poly
circle origin radius col points = let
	step = 2 * pi / fromIntegral points
	halfpi = 0.5 * pi
	angles = [halfpi, halfpi + step .. 2 * pi]
	in Poly origin (map (angleToVec origin radius) angles) col

square :: Vec -> Float -> Col -> Poly
square (Vec x y) a c =
	rectangle (Vec (x-a/2) (y-a/2)) (Vec (x+a/2) (y+a/2)) c

rectangle :: Vec -> Vec -> Col-> Poly
rectangle p0 p1 col = let
	(Vec x0 y0) = p0
	(Vec x1 y1) = p1
	in Poly (Vec ((x0+x1)/2) ((y0+y1)/2)) [p0, Vec x0 y1, p1, Vec x1 y0] col

drawPrimitive :: PrimitiveMode -> [Vec] -> Col -> IO ()
drawPrimitive primMode vecs col = do
	setColor col
	renderPrimitive primMode $ mapM_ vertexVec vecs
