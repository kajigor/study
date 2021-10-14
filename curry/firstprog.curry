{-# OPTIONS_CYMAKE -F --pgmF=currypp --optF=defaultrules #-}

nine = 3 * 3 

square x = x * x 

absolute x = if x >= 0 then x else -x 

fac n = if n == 0 then 1 else n * fac (n-1)

False &&& _ = False 
True &&& x = x 

False ||| x = x 
True ||| _ = True 

myNot False = True 
myNot True = False

pneg True = False 

choose x y = x 
choose x y = y  

last (_++[e]) = e 

secondToLast (_++[e,_]) = e
-- non-deterministic: myZip [1,2,3] [4,5] gives 
-- three possible solutions [(1,4),(2,5)], [(1,4)], []
-- myZip (x:xs) (y:ys) = (x,y) : myZip xs ys
-- myZip _ _ = []


myZip (h:t) (h1:t1) = (h,h1) : myZip t t1
myZip'default _ _ = [] 

foo x = x ? x + 1 
