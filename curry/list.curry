data List a = Nil | Cons a (List a) 

length :: List a -> Int 
length Nil = 0 
length (Cons _ t) = 1 + length t 

elem :: Eq a => a -> List a -> Bool 
elem _ Nil = False 
elem x (Cons h t) = h == x || elem x t  
