my_reverse: List a -> List a
my_reverse [] = []
my_reverse (x :: xs) = my_reverse xs ++ [x]
