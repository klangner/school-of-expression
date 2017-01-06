module Shape ( Radius
             , Shape
             , Side
             , Vertex
             , area
             , circle
             , rectangle
             , regularPolygon
             , rtTriangle
             , square
             )where

type Radius = Float
type Side = Float
type Vertex = (Float, Float)

-- | Basic shapes
data Shape = Rectangle Side Side
           | Ellipse Radius Radius
           | RtTriangle Side Side
           | Polygon [Vertex]
           deriving (Show)


-- | Unit square constructor
square :: Shape
square = Rectangle 1 1


-- | Regular polygon constructor
regularPolygon :: Int    -- ^ Number of vertices
               -> Side   -- ^ Side length
               -> Shape
regularPolygon n s = Polygon (map (\p -> (cos p, sin p)) ps)
    where ps = [2*pi*(fromIntegral k)/(fromIntegral n) | k <- [0..(n - 1)]]


-- | Circle constructor
circle :: Radius -> Shape
circle r = Ellipse r r


-- | Right triangle constructor
rtTriangle :: Side -> Shape
rtTriangle  s = RtTriangle s s


-- | Rectangle constructor
rectangle :: Side       -- ^ Width
          -> Side       -- ^ Height
          -> Shape
rectangle w h = Rectangle w h


-- | Calculare shape area
area :: Shapre -> Float
area (Rectangle w h) = w*h
area (Ellipse r1 r2) = 1
area (RtTrangle s1 s2) = s1*s2/2
area (Polygon vs) = 1