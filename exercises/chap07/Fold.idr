totalLen : List String -> Nat
totalLen xs = foldr (\str, len => length str + len) 0 xs


data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)

Foldable Tree where
  foldr func acc Empty = acc
  foldr func acc (Node left e right)
          = let leftfold = foldr func acc left
                rightfold = foldr func leftfold right in
                func e rightfold
