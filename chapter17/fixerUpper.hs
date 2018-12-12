
a1 = const <$> Just "Hello" <*> pure "World"

a2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]

main :: IO ()
main = do
  putStrLn (show a1)
  putStrLn (show a2)
