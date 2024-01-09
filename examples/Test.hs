f :: [a] -> [a]
f xs = ys ++ ys
  where ys = reverse xs
