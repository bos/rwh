module GoodTree where

import Tree

{-- snippet nodesAreSame --}
nodesAreSame (Node a _ _) (Node b _ _)
    | a == b     = Just a
nodesAreSame _ _ = Nothing
{-- /snippet nodesAreSame --}
