data Tree a = Leaf | Node a (Tree a) (Tree a)

findCurry Leaf = False 
findCurry (Node x l r) = x == "Curry" || findCurry l || findCurry r

type IntTree = Tree Int 

total :: IntTree -> Int 
total Leaf = 0 
total (Node x l r) = x + total l + total r 
