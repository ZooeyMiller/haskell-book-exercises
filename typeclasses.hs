--data Pair a =
--  Pair a a

--instance (Eq a) => Eq (Pair a) where
--   (==) (Pair a b) (Pair x y) = a == x && b == y 

--data Tuple a b = 
--  Tuple a b

--instance (Eq a, Eq b) => Eq (Tuple a b) where
--  (==) (Tuple a b) (Tuple x y) = a == x && b == y

--data Which a = 
--  ThisOne a
--  | ThatOne a

--instance (Eq a) => Eq (Which a) where
--  (==) (ThisOne a) (ThisOne b) = a == b
--  (==) (ThatOne a) (ThisOne b) = a == b
--  (==) (ThatOne a) (ThatOne b) = a == b
--  (==) (ThisOne a) (ThatOne b) = a == b

data EitherOr a b = 
  Hello a
  | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a) (Hello b) = a == b
  (==) (Goodbye a) (Goodbye b) = a == b
  (==) _ _ = False 
