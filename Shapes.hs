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

	vecAngle :: Vec -> Float
	vecAngle (Vec x y) = atan (y / x) + addition
		where addition = if x < 0 then pi else if y < 0 then 2 * pi else 0

	data Col = Col Float Float Float deriving Show
	setColor :: Col -> IO ()
	setColor (Col r g b) = do
		color $ Color3 r g (b :: GLfloat)

	data FilledPolygon = FilledPolygon [Vec] Col | Circle Vec [Vec] Col
	drawFilledPolygon :: FilledPolygon -> IO ()
	drawFilledPolygon (FilledPolygon points col) = do
		drawPrimitive Polygon points col
	drawFilledPolygon (Circle _ points col) = do
		drawPrimitive Polygon points col

	circle :: Vec -> Float -> Col -> Int -> FilledPolygon
	circle center r col points = let
		(Vec x0 y0) = center;
		step = 2 * pi / fromIntegral points
		angles = [0, step .. 2 * pi]
		pointAtAngle a = Vec (x0 + r * (cos a)) (y0 + r * (sin a))
		in Circle center (map pointAtAngle angles) col

	rectangle :: Vec -> Vec -> Col-> FilledPolygon
	rectangle (Vec x0 y0) (Vec x1 y1) col =
		FilledPolygon [Vec x0 y0, Vec x0 y1, Vec x1 y1, Vec x1 y0] col

	drawPrimitive :: PrimitiveMode -> [Vec] -> Col -> IO ()
	drawPrimitive primMode vecs col = do
		setColor col
		renderPrimitive primMode $ mapM_ vertexVec vecs
