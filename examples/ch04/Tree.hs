{-- snippet Tree --}
data Tree a = Node (Tree a) (Tree a)
            | Leaf a
              deriving (Show)

{-- /snippet Tree --}

{-- snippet ComplexTree --}
data ComplexTree a b = ComplexNode a (ComplexTree a b) (ComplexTree a b)
                     | ComplexLeaf b
                       deriving (Show)
{-- /snippet ComplexTree --}
