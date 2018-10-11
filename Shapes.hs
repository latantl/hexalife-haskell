module Shapes where

	import Graphics.UI.GLUT hiding (Point)

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

	data Col = Col Float Float Float deriving Show
	setColor (Col r g b) = do
		color $ Color3 r g (b :: GLfloat)

	data Circle =
		Circle { origin :: Vec, radius :: Float, col :: Col} deriving Show
	circlePoints :: Circle -> Int -> [Vec]
	circlePoints (Circle (Vec x0 y0) r _) points
		| points < 3 = []
		| otherwise = let
			step = 2 * pi / fromIntegral points
			angles = [0, step .. 2 * pi]
			pointAtAngle a = Vec (x0 + r * (sin a)) (y0 + r * (cos a))
			in map pointAtAngle angles
	drawCircle :: Circle -> Int -> IO ()
	drawCircle c acc = renderPrimitive Polygon $ do
		let (Circle _ _ rgb) = c
		setColor rgb
		mapM_ vertexVec (circlePoints c acc)

	data Rectangle = Rectangle Vec Vec Col deriving Show
	drawRectangle :: Rectangle -> IO ()
	drawRectangle (Rectangle (Vec x0 y0) (Vec x1 y1) c) = do
		setColor c
		renderPrimitive Polygon $ do
			vertexVec (Vec x0 y0)
			vertexVec (Vec x0 y1)
			vertexVec (Vec x1 y1)
			vertexVec (Vec x1 y0)
