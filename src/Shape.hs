module Shape where

type Radius = Float
type Side = Float
type Vertex = (Float, Float)

data Shape = Rectangle Side Side
           | Ellipse Radius Radius
           | RtTriangle Side Side
           | Polygon [Vertex]
           deriving (Show)

square :: Shape
square = Rectangle 1 1

regularPolygon :: Int -> Side -> Shape
regularPolygon n s = Polygon (map (\p -> (cos p, sin p)) ps)
    where ps = [2*pi*(fromIntegral k)/(fromIntegral n) | k <- [0..(n - 1)]]

