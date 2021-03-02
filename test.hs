import Text.Printf

xs = [1,2,3]
(y:ys) = xs

oddsRec :: [Int] -> [Int]
oddsRec [] = []
oddsRec (x:xs) = if mod x 2 == 1 then x: oddsRec xs else oddsRec xs

sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs

--prop_a xs  =  maximum xs == foldr (`max`) 0 xs
prop_b xs  =  sum xs == foldr (+) 0 xs
prop_c xs  =  product xs == foldr (*) 1 xs
prop_d xs  =  and xs == foldr (&&) True xs
prop_e xs  =  concat xs == foldr (++) [] xs


he = foldr (*) 1 . map cube . filter neg
    where
        cube x  =  x * x * x
        neg x   =  x < 0


hd [] = 1
hd (x:xs) | x < 0     = x * x * x * hd xs
         | otherwise = hd xs


hc xs = foldr (*) 1 (filter neg (map cube xs))
    where
        cube x  =  x * x * x
        neg x   =  x < 0


hb xs = foldr (*) 1 (map cube (filter neg xs))
    where
        cube x  =  x * x * x
        neg x   =  x < 0

ha xs = product [ x*x*x | x <- xs, x < 0 ]

prop_check xs = (ha xs == hb xs) && (hb xs == hc xs) && (hc xs == hd xs) && (hd xs == he xs)

data Shape = Circle Float | Rect Float Float
 
eqShape :: Shape -> Shape -> Bool
eqShape (Circle r) (Circle r')   =  (r == r')
eqShape (Rect w h) (Rect w' h')  =  (w == w') && (h == h')
eqShape x          y             =  False

map' f ys  =
  if null ys then
    []
  else
    let
      x = head ys
      xs = tail ys
    in
      f x : map' f xs


showOp f = f (printf "(%s op %s)") "0" ["1","2","3"]


data Atom = A|B|C|D|P|Q|R|S|W|X|Y|Z deriving (Show)
data Wff a = V a
 | Not (Wff a)
 | Wff a :&: Wff a
 | Wff a :|: Wff a

par :: String -> String
par x  =  "(" ++ x ++ ")"

showWff :: Show a => Wff a -> String
showWff (V a)      =  show a
showWff (Not p)    =  par ("~" ++ showWff p)
showWff (p :&: q)  =  par (showWff p ++ "&" ++ showWff q)
showWff (p :|: q)  =  par (showWff p ++ "|" ++ showWff q)




