import Data.Map (Map)
import qualified Data.Map as Map

data AExp = Var String | Val Int | Add AExp AExp | Mult AExp AExp | Custom String AExp AExp deriving(Show)
data BExp = BTrue | BFalse | Less AExp AExp | And BExp BExp | Not BExp deriving(Show)
data Prog = Skip | P String | V String | Let String AExp | Seq Prog Prog | Cond BExp Prog Prog | While BExp Prog | Para Prog Prog deriving(Show)


data Graph = Graph
  {nodes :: Int
  , edges :: [[Int]] } deriving(Show)

edgevis :: [[Int]] -> IO()
edgevis [] = return ()
edgevis (x:xs) = let vs = foldr (\a b -> show a ++ if b == "" then b else " " ++ b) "" x in do
  putStrLn vs
  edgevis xs
graphvis :: Graph -> IO()
graphvis (Graph n e) = do
  edgevis (map (\n -> [n]) [0..n - 1])
  edgevis e

trgraph :: Prog -> Graph
trgraph Skip = Graph 1 []
trgraph (P _) = Graph 2 [[0,1]]
trgraph (V _) = Graph 2 [[0,1]]
trgraph (Let _ _) = Graph 2 [[0,1]]
trgraph (Seq p1 p2) = let g1 = trgraph p1 in
  let g2 = trgraph p2 in
  Graph (nodes g1 + nodes g2 - 1) (edges g1 ++ (map (\e -> map (\n -> n + nodes g1 - 1) e) (edges g2)))
trgraph (Cond _ p1 p2) = let g1 = trgraph p1 in
  let g2 = trgraph p2 in
  Graph (nodes g1 + nodes g2) ([[0,1],[0,nodes g1]] ++ (map (\e -> map (\n -> if n == nodes g1 - 1 then nodes g1 + nodes g2 - 1 else n + 1) e) (edges g1)) ++ (map (\e -> map (\n -> n + nodes g1) e) (edges g2)))
trgraph (While _ p) = let g = trgraph p in
  Graph (nodes g + 1) ([[0,nodes g - 1],[0,nodes g]] ++ (map (\e -> map (\n -> nodes g - 1 - n) e) (edges g)))
trgraph (Para p1 p2) = let g1 = trgraph p1 in
  let g2 = trgraph p2 in
  let leib1 = map (\i -> map (\e -> map (\n -> n + (i * nodes g1)) e) (edges g1)) [0..nodes g2 - 1] in
  let leib2 = map (\i -> map (\e -> map (\n -> i + (n * nodes g1)) e) (edges g2)) [0..nodes g1 - 1] in
  Graph (nodes g1 * nodes g2) (concat leib1 ++ concat leib2)

work_zero :: Map String Int -> Bool
work_zero = (foldr (\a b -> b && (a == 0)) True) . Map.elems
work_eq :: Map String Int -> Map String Int -> Bool
work_eq a b = (work_zero (a Map.\\ b)) && (work_zero (b Map.\\ a))
  && (work_zero (Map.intersectionWith (-) a b))

work :: Prog -> Maybe (Map String Int)
work Skip = Just Map.empty
work (Let _ _) = Just Map.empty
work (P a) = Just (Map.singleton a (-1))
work (V a) = Just (Map.singleton a 1)
work (Seq p q) = do 
  wp <- work p
  wq <- work q
  Just (Map.unionWith (+) wp wq)
work (Cond _ p q) = do
  wp <- work p
  wq <- work q
  if (work_eq wp wq) then work p else Nothing
work (While _ p) = do
  wp <- work p
  if (work_zero wp) then Just Map.empty else Nothing
work (Para p q) = do
  wp <- work p
  wq <- work q
  Just (Map.unionWith (+) wp wq)

b_or :: BExp -> BExp -> BExp
b_or b1 b2 = Not (And (Not b1) (Not b2))
b_neq :: AExp -> AExp -> BExp
b_neq a1 a2 = b_or (Less a1 a2) (Less a2 a1)


syracuse = Seq 
  (Let "x" (Val 5)) 
  (Seq 
    (While (b_neq (Var "x") (Val 1)) 
      (Cond (b_neq (Custom "mod" (Var "x") (Val 2)) (Val 0)) 
        (Seq
          (Let "x" (Mult (Val 3) (Var "x"))) 
          (Let "x" (Add (Var "x") (Val 1)))) 
        (Let "x" (Custom "/" (Var "x") (Val 2)))))
    (Let "print" (Custom "string:[Reached 1]" (Val 0) (Val 0))))

scale = Seq
  (Let "d" (Val 1))
  (Para 
    (Seq (P "a") (Seq (Let "d" (Val 0)) (V "a")))
    (Seq (P "a") (Seq 
      (Cond (b_neq (Var "d") (Val 0)) 
        (Let "y" (Custom "/" (Var "x") (Var "d"))) 
        (Let "y" (Var "x"))) 
      (V "a"))))

block :: String -> Prog -> Prog
block a p = Seq (P a) (Seq p (V a))


main = do 
  graphvis (trgraph syracuse)
  print (work syracuse)
  print (work scale)
  print (work (While BTrue (block "a" Skip)))
