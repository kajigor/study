{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

module Chapter3.Heap where 

class Heap t a where 
  empty :: t a 
  isEmpty :: t a -> Bool 
  insert :: a -> t a -> t a 
  merge :: Ord a => t a -> t a -> t a 
  findMin :: t a -> Maybe a 
  deleteMin :: t a -> t a 

data LeftBiasedHeap a = Empty | Heap Int a (LeftBiasedHeap a) (LeftBiasedHeap a)
                      deriving (Show, Eq, Ord)

instance Ord a => Heap LeftBiasedHeap a where 
  empty = Empty 
  
  isEmpty Empty = True 
  isEmpty _ = False 

  merge h Empty = h 
  merge Empty h = h 
  merge h1@(Heap _ x a1 b1) h2@(Heap _ y a2 b2) = 
      if x <= y 
      then makeT x a1 (merge b1 h2)
      else makeT y a2 (merge h1 b2)
  
  insert x h = merge (Heap 1 x Empty Empty) h 

  findMin Empty = Nothing 
  findMin (Heap _ x _ _) = Just x 

  deleteMin Empty = Empty 
  deleteMin (Heap _ _ a b) = merge a b 
  
makeT x a b = 
    let rankA = rank a in 
    let rankB = rank b in 
    if rankA >= rankB 
    then Heap (rankB + 1) x a b 
    else Heap (rankA + 1) x b a 
rank Empty = 0 
rank (Heap r _ _ _) = r 

insert' :: Ord a => a -> LeftBiasedHeap a -> LeftBiasedHeap a 
insert' x Empty = Heap 1 x Empty Empty 
insert' x (Heap d y l r) = 
    makeT (min x y) l (insert' (max x y) r)

computeRank :: Ord a => LeftBiasedHeap a -> Int 
computeRank Empty = 0 
computeRank (Heap _ _ _ b) = 1 + computeRank b

isLeftist :: Ord a => LeftBiasedHeap a -> Bool 
isLeftist Empty = True 
isLeftist (Heap _ _ a b) = 
    computeRank a >= computeRank b 
    && isLeftist a 
    && isLeftist b  

isHeap :: Ord a => LeftBiasedHeap a -> Bool 
isHeap Empty = True 
isHeap (Heap _ x l r) = 
    checkHeap x l && checkHeap x r 
  where 
    checkHeap _ Empty = True 
    checkHeap x h@(Heap _ y _ _) = x <= y && isHeap h 
   

