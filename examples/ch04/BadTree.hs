module BadTree where

import Tree

{-- snippet nodesAreSame --}
nodesAreSame (Node a _ _) (Node a _ _) = Just a
nodesAreSame _            _            = Nothing
{-- /snippet nodesAreSame --}
