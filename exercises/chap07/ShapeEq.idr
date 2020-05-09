data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle base height) = base * height
area (Circle radius) = pi * radius * radius

Eq Shape where
  (==) (Triangle a b) (Triangle a' b')
        = a == a' && b == b'
  (==) (Rectangle a b) (Rectangle a' b')
        = a == a' && b == b'
  (==) (Circle a) (Circle a')
        = a == a'
  (==) _ _
        = False

Ord Shape where
  compare a b
    = compare (area a) (area b)

testShapes : List Shape
testShapes = [Circle 3, Triangle 3 9, Rectangle 2 6, Circle 4, Rectangle 2 7]
