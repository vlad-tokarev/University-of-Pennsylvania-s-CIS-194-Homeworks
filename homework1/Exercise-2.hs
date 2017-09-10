import CodeWorld

main :: IO ()
main = exercise2



-- Exercise 2

tree :: Integer -> Picture -> Picture
tree n p
  | n == 0 = blank
  | n == 1 = path [(0,0),(0,1)] & p
  | otherwise = path [(0,0),(0,1)] & translated 0 1 (
  rotated (pi/10) (tree (n-1) p) & rotated (- pi/10) (tree (n-1) p)) 
  
bloom :: Double -> Picture
bloom size = translated 0 1 (colored green (solidCircle size))

anim :: Double -> Picture
anim t 
  | t < 10 = tree 8 (bloom (2/100 * t))
  | otherwise = tree 8 (bloom (2/10))

exercise2 :: IO ()
exercise2 = animationOf  anim



 