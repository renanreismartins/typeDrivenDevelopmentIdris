import Data.Vect

my_map_vec : (a -> b) -> Vect n a -> Vect n b
my_map_vec f [] = []
my_map_vec f (x :: xs) = f x :: my_map_vec f xs
