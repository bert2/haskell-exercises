import Network.URI.Encode

data Tree a = Node Integer (Tree a) a (Tree a)
            | Leaf
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

insert :: a -> Tree a -> Tree a
insert x Leaf                 = Node 1 Leaf x Leaf
insert x (Node n l v r)
    | numNodes l < numNodes r = Node (n+1) (insert x l) v r
    | otherwise               = Node (n+1) l v (insert x r)

numNodes :: Tree a -> Integer
numNodes Leaf           = 0
numNodes (Node n _ _ _) = n

treeBallance :: Tree a -> (Integer, Integer)
treeBallance Leaf           = (0,0)
treeBallance (Node _ l _ r) = (numNodes l, numNodes r)

toDotGraph :: Show a => Tree a -> String
toDotGraph Leaf = ""
toDotGraph t    =
    let dot = "digraph Tree {" ++ toDotGraph' t ++ "}"
    in "https://g.gravizo.com/svg?" ++ encode dot
    where toDotGraph' Leaf             = ""
          toDotGraph' t@(Node _ l _ r) = trans "l" t l ++ toDotGraph' l ++
                                         trans "r" t r ++ toDotGraph' r
          trans which p@(Node _ _ _ _) c@(Node _ _ _ _) =
              node p ++ "->" ++ node c ++ "[label=" ++ which ++ "];\n"
          trans _     p@(Node _ _ _ _) _                = node p ++ ";"
          node (Node h _ v _) = "_" ++ show v ++ "_" ++ show h
          node _ = ""
