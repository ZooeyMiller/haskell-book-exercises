-- a
a = (+ 1) <$> (read "[1]" :: [Int])
-- original: a = (+1) $ read "[1]" :: [Int] 
-- Prelude> a
-- [2]

-- b
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])
-- original: b = (++ "lol") (Just ["Hi,", "Hello"]) 
-- Prelude> b
-- Just ["Hi,lol","Hellolol"]

-- c
c = (* 2) . (\x -> x - 2)
-- original: c = (*2) (\x -> x - 2)
-- Prelude> c 1
-- -2

-- d
d = ((return '1' ++) . show) <$> (\x -> [x, 1 .. 3])
-- original: d = ((return '1' ++) . show) (\x -> [x, 1..3])
-- Prelude> d 0
-- "1[0,1,2,3]"

-- e
e :: IO Integer
e =
  let ioi     = readIO "1" :: IO Integer
      changed = read <$> ("123" ++) <$> show <$> ioi :: IO Integer
  in  (* 3) <$> changed
-- original: 
-- e :: IO Integer
-- e = 
--     let ioi = readIO "1" :: IO Integer
--         changed = read ("123"++) show ioi in (*3) changed
-- Prelude> e
-- 3693
