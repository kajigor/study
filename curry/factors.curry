infix 7 ./.

_ ./. 0 = False 
x ./. y | y /= 0 = x `mod` y == 0 

-- -- etalon solution
-- x ./. y = y /= 0 && x `mod` y == 0 

-- -- nondeterministic: patterns are not disjunctive
-- _ ./. 0 = False 
-- x ./. y = y /= 0 && x `mod` y == 0 

