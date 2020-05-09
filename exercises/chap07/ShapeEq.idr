data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

Eq Shape where
  (==) (Triangle a b) (Triangle a' b')
        = a == a' && b == b'
  (==) (Rectangle a b) (Rectangle a' b')
        = a == a' && b == b'
  (==) (Circle a) (Circle a')
        = a == a'
  (==) _ _
        = False
