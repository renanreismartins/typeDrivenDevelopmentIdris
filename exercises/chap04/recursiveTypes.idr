data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

data Picture = Primitive Shape
             | Combine Picture Picture
             | Rotate Double Picture
             | Translate Double Double Picture

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Circle radius) = pi * radius * radius

--biggest area
biggerArea : Shape -> Shape -> Shape
biggerArea x y = if (area x) > (area y) then x
                                        else y

-- map to shapes
reducePictures : Picture -> List Shape
reducePictures (Primitive shape) = [shape]
reducePictures (Combine pic1 pic2) = [] ++ (reducePictures pic1) ++ (reducePictures pic2)
reducePictures (Rotate x pic) = [] ++ reducePictures pic
reducePictures (Translate x y pic) = [] ++ reducePictures pic

filterTriangles : List Shape -> List Shape
filterTriangles [] = []
filterTriangles (x :: xs) = case x of
                                 (Triangle _ _) => x :: filterTriangles xs
                                 (Shape) => [] ++ filterTriangles xs


biggestShapeAux : Shape -> List Shape -> Maybe Shape
biggestShapeAux x [] = Just x
biggestShapeAux x (y :: xs) = let biggerShape = biggerArea x y in
                                  biggestShapeAux biggerShape xs

biggestShape : List Shape -> Maybe Shape
biggestShape [] = Nothing
biggestShape (x :: xs) = biggestShapeAux x xs



biggestTriangle : Picture -> Maybe Double
biggestTriangle pic  = let pictures = reducePictures pic
                           triangles = filterTriangles pictures
                           maybeBiggest = biggestShape triangles in
                           case maybeBiggest of
                                Nothing => Nothing
                                (Just shape) => Just(area shape)


testPic1 : Picture
testPic1 = Combine (Primitive (Triangle 2 3))
                   (Primitive (Triangle 2 4))

testPic2 : Picture
testPic2 = Combine (Primitive (Rectangle 1 3))
                   (Primitive (Circle 4))
