data BSTree : Type -> Type where
     Empty : Ord elem => BSTree elem
     Node : Ord elem => (left : BSTree elem) -> (val : elem) -> (right : BSTree elem) -> BSTree elem

insert : elem -> BSTree elem -> BSTree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node left val right)
      = case compare x val of
             LT => Node (insert x left) val right
             EQ => orig
             GT => Node left val (insert x right)



listToTree : Ord a => List a -> BSTree a
listToTree [] = Empty
listToTree (x :: xs) = let inserted = listToTree xs in
                           insert x inserted



treeToList : BSTree a -> List a
treeToList Empty = []
treeToList (Node left val right) = let insertedLeft = treeToList left
                                       insertedRight = treeToList right in
                                       insertedLeft ++ (val :: insertedRight)
