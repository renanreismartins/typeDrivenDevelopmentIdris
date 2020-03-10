overLength : Nat -> List String -> Nat
overLength n words = length (filter overN words)
  where
    overN : String -> Bool
    overN e = length e > n

