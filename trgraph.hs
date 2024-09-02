data Prog = Skip | Act String | Seq Prog Prog | Cond String Prog Prog | While String Prog | Para Prog Prog deriving(Show)

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
trgraph (Act _) = Graph 2 [[0,1]]
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

syracuse = Seq (Act "x := 5") (Seq (While "x != 1" (Cond "x mod 2 != 0" (Seq (Act "x := 3 * x") (Act "x := x + 1")) (Act "x := x/2"))) (Act "print 'Reached 1!'"))

main = graphvis (trgraph syracuse)
