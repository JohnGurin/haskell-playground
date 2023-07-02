transpose :: [[a]] -> [[a]]
transpose ([] : _) = []
transpose x = map head x : transpose (map tail x)

main =
  print (transpose [[1, 2], [3, 4], [5, 6]]) -- [[1,3,5],[2,4,6]]