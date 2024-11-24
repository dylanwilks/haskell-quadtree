{-# LANGUAGE ScopedTypeVariables #-}
import Control.Exception --Both imports used for catching errors
import System.IO.Unsafe

--Most definitions copied from submission.hs

--Algebraic data types--

  --color to correctly compare cells
data Color = White | Black 
  deriving (Eq, Show)

  --quadtree recursive data constructors; Cell represents the base case of 1 color
data QuadTree = Cell Color | Quadrants QuadTree QuadTree QuadTree QuadTree
  deriving (Eq, Show)

--Type constructors--

  --for returning cells of the given color
allBlack :: QuadTree 
allWhite :: QuadTree

  --for checking arrangement of quadtrees;
  --takes four QuadTree types as input and returns a QuadTree parent node of the given inputs
clockwise :: QuadTree -> QuadTree -> QuadTree -> QuadTree -> QuadTree
anticlockwise :: QuadTree -> QuadTree -> QuadTree -> QuadTree -> QuadTree


--Function definitions--

  --return QuadTree type with Cell data constructor of corresponding color
allBlack = Cell Black
allWhite = Cell White

  --use Quadrants data constructor to return QuadTree parent node of 4 inputs.
clockwise q1 q2 q3 q4 = Quadrants q1 q2 q3 q4
anticlockwise q1 q2 q3 q4 = Quadrants q1 q4 q3 q2


--Open-ended exercise solution attempt--

--Note: all the comments here on out will be talking about ergodomestic quadtrees by default;
--I recognize that all quadtrees not consisting of errors/bottoms/undefined types are ergodomestic.

  --Target solution
my_solution :: (QuadTree -> Bool) -> QuadTree


--Problem 1: Brute forcing for a solution (assuming there exists one). 
--This uses cyclic multi-digraphs to encode both finite and infinite quadtrees so we can enumerate them.

  --Node data type to use for digraphs:
  --Node k represents 'Quadrants q1 q2 q3 q4', where q1...q4 will be represented by 4 outgoing edges.
  --T Color represents Cell White or Cell Black (these nodes won't have outgoing edges).
data Node = Node Int | T Color
  deriving (Eq, Show)


--Type constructors for Problem 1--

  --Supplementary function used for evaluating the Quadtrees one at a time with the fair exercise,
  --returning a quadtree it returns true for.
check_true :: (QuadTree -> Bool) -> [QuadTree] -> QuadTree

  --combinations: takes an integer n and a list of (unique) nodes then generates a list
  --of every possible n-tuple lists consisting of them.
  --
  --graphs: takes an integer n representing the amount of Node k objects (k <- [1..n])
  --and then produces a list containing lists of every possible edge set bound by the rules
  --provided by the comments on lines 51 and 52.
combinations :: Int -> [Node] -> [[Node]]
graphs :: Int -> [[(Node, Node)]]

  --graphtoQuadtree: this takes a vertex (node) from the graph (where we start building a tree)
  --and the edge set and starts building the quadtree from there. 
graphtoQuadtree :: Node -> [(Node, Node)] -> QuadTree


--Function definitions for Problem 1--

combinations 0 vs = [[]] --base case
combinations n vs = [x:xs | x <- vs, xs <- combinations (n-1) vs] --recursively consider all the combinations of
                                                                  --the tail of the list and for each combination
                                                                  --generate several more by appending one of the 
                                                                  --values in vs to the head for all values in vs.

graphs n = [[(Node k, m !! (index k i)) | k <- [1..n], i <- [1..4]]  --as each node has 4 outgoing edges generate
                                        | m <- combinations (4*n) d] --the set 'm' of all combinations of size 4n
  where                                                              --with all possible node elements (including  
    d = concat [[Node k | k <- [1..n]], [T Black, T White]]          --color nodes). Then for each combination in
    index = \x y -> (4*(x-1)) + (y-1)                                --m generate edge sets where each node    
                                                                     --(excluding color nodes) has 4 outgoing     
                                                                     --edges with elements in the combination     
                                                                     --having inbound edges. The index function
                                                                     --simply associates Node k with the inbound
                                                                     --edges in the combination set.
                                                                    
  --Note: graphs n may return plenty of graphs that will yield duplicate quadtrees; some graphs 
  --won't have a path between all nodes either. Regardless, the set remains finite and every possible
  --quadtree encoded by the graphs with n nodes (including graphs in graphs k for k < n) is included, 
  --so the solution will still be found in finite time. Be aware that time taken to generate graphs 
  --drastically increases for n > 2 as the jump from n=2 to n=3 for the combinations function goes from 
  --around 65000 to over 240000000, then to around 2.8*10^12 for n=4.

graphtoQuadtree (T Black) edges = allBlack 
graphtoQuadtree (T White) edges = allWhite --base cases

graphtoQuadtree (Node k) edges =
  clockwise (graphtoQuadtree (snd (edges !! (4*(k-1)))) edges)   --starting from the given Node k create a 
            (graphtoQuadtree (snd (edges !! (4*(k-1)+1))) edges) --Quadtree with 4 children. The 4 children   
            (graphtoQuadtree (snd (edges !! (4*(k-1)+2))) edges) --are the corresponding elements with inbound
            (graphtoQuadtree (snd (edges !! (4*(k-1)+3))) edges) --edges from Node k. Then apply recursively. 
                                                                 
check_true f (x:xs) = if f x == True       --simply return x if the boolean function returns true on it.
                      then x               --otherwise apply to the rest of the list. As the list is infinite 
                      else check_true f xs --we do not need to consider an empty list.                        
                                            


--Problem 2: Checking if a solution exists.
--This uses the fact that fair exercises halt, meaning that they won't pattern match indefinitely 
--(as a consequence if the fair exercise returns true on an infinite quadtree, then it must return true 
--for some finite quadtree as well). This solution abuses errors by placing them at a certain depth (n) on
--a set of quadtrees and checking if the fair exercise yields an error or not. If no error is yielded this
--means that the given fair exercise only pattern matches up to a depth of (n-1).


--Type constructors for Problem 2--

  --replaceLeaves: takes an integer n, a quadtree l and another quadtree q. This function will replace
  --all nodes of depth n in quadtree q with quadtree l.
  --
  --finiteTrees: takes an integer n and generates the list of all quadtrees with height n.
  --
  --error_list: takes an integer n and uses finiteTrees and replaceLeaves to generate a list
  --of quadtrees of height n where all nodes of depth n are an error.
replaceLeaves :: Int -> QuadTree -> QuadTree -> QuadTree
finiteTrees :: Int -> [QuadTree]
error_list :: Int -> [QuadTree]

  --halt_error: the crux of the algorithm. Takes a boolean function f and a quadtree q. 
  --If f q yields an error return True. Otherwise return False (even if f q == True).
  --
  --safe_depth: Takes a boolean function f and an integer n.
  --Then performs a map on error_list n using halt_error f. If no error is yielded for
  --the value n then return (n-1); the depth f pattern matches up to.
halt_error :: (QuadTree -> Bool) -> QuadTree -> Bool
safe_depth :: (QuadTree -> Bool) -> Int -> Int

  --Takes a boolean function f and uses the value yielded by safe_depth to
  --generate a (finite) set of quadtrees to check for a solution for f.
solution_exists :: (QuadTree -> Bool) -> Bool


replaceLeaves 0 q (Cell _) = q                --base cases; only replace nodes with a height of 0.
replaceLeaves _ q (Cell White) = Cell White
replaceLeaves _ q (Cell Black) = Cell Black
replaceLeaves height leaf (Quadrants q1 q2 q3 q4) = 
  Quadrants (replaceLeaves (height-1) leaf q1)  --for each node with 4 children perform the function
            (replaceLeaves (height-1) leaf q2)  --recursively on each one with height-1. Will 
            (replaceLeaves (height-1) leaf q3)  --clearly terminate if the quadtree is not infinite
            (replaceLeaves (height-1) leaf q4)  --(it will eventually reach a cell).                 
                                                
                                                
--Function definitions for Problem 2--                                                

finiteTrees 0 = [allWhite, allBlack]  --base case 
finiteTrees n = [Quadrants q1 q2 q3 q4 | q1 <- d,      --uses elements from previous trees (d) to 
                                         q2 <- d,      --build new trees. q `elem` k is used to 
                                         q3 <- d,      --ensure that at least one of the subtrees
                                         q4 <- d,      --has height n-1 and hence ensuring all
                                         q1 `elem` k   --the trees generated have height n.
                                      || q2 `elem` k
                                      || q3 `elem` k
                                      || q4 `elem` k]
  where 
    d = concat [finiteTrees s | s <- [0..(n-1)]]
    k = finiteTrees (n-1)                              
                                                       
error_list 0 = [error ""]                                              --replaces the leaves with depth n in
error_list 1 = [Quadrants (error "") (error "") (error "") (error "")] --finiteTrees n with errors. We have to
error_list n = map (replaceLeaves n (error "")) (finiteTrees n)        --consider every case as lazy evaluation
                                                                       --may cause the fair exercise to terminate
                                                                       --safely due to mismatching spines of other
                                                                       --nodes despite there being some nodes that
                                                                       --would be compared with an error.
halt_error f q = 
  unsafePerformIO (evaluate (if (f q == False) || (f q == True) --if f q evaluates to either False or True
                             then False                         --(terminates safely) then return False.
                             else False)                        --Otherwise use an anonymous handler function
                   `catch` \(_ :: ErrorCall) -> pure True)      --to catch the error and return True.
                                                           
safe_depth f n = if foldr (||) False (map (halt_error f) (error_list n)) --checks if error_list n yields
                 then safe_depth f (n+1)                                 --any errors by using halt_error
                 else (n-1)                                              --with foldr using || so that
                                                                         --if any of the elements in
                                                                         --error_list return True (an error)
                                                                         --foldr returns true immediately
                                                                         --thanks to lazy evaluation.
                                                                         --If so perform recursively for n+1,
                                                                         --but if no errors are found (foldr
                                                                         --returns False) then return n-1.

  --Note: due to the sheer size of finiteTrees 3, if the fair exercise pattern matches beyond a depth of 1
  --then this function may run for a long time. Regardless, it can be argued that it terminates in finite time:
  --if finiteTrees n is finite for any n then clearly safe_depth terminates in finite time as error_list n will
  --be finite (we have argued already that replaceLeaves terminates on finite trees).
  --By (strong) induction we can say that if finiteTrees k (k <- [0..n]) is finite then so is
  --finiteTrees n+1 since each subtree of trees in the list will be elements of finiteTrees k
  --and the list comprehension ensures each tree is unique. As finiteTrees k is finite 
  --for (k <- [0..2]) (simply through testing) this means it terminates for all n >= 0.

solution_exists f = let depth = safe_depth f 0                                  --starting from n=0 perform
                    in if depth == (-1)                                         --safe_depth f n to yield a
                       then False                                               --depth value of trees that
                       else foldr (||) False (map f ((allWhite):(allBlack)      --need to be searched.
                            :(map (graphtoQuadtree (Node 1)) (graphs depth))))  --If depth = -1 this means that
                                                                                --f returns True or False on
                                                                                --everything without pattern
                                                                                --matching (_) so return False
                                                                                --(so anything can be returned
                                                                                --later). Otherwise using graphs
                                                                                --check if f does return true
                                                                                --for any quadtree consisting
                                                                                --of depth # of nodes (finite). 
                                                                                --Then return true if there 
                                                                                --exists at least a single case.
                                                                               
my_solution f = if solution_exists f
                then check_true f ((allWhite):(allBlack):
                     (concat [(map (graphtoQuadtree (Node 1)) (graphs k)) | k <- [1..]]))
                else allWhite               --If solution_exists == True brute force all solutions starting from
                                            --a depth of 0 (as there still may be a solution here). Otherwise
                                            --just return allWhite. Recall this may return infinite trees.

fair_exercise (Quadrants (Cell Black) (Cell White) (Cell Black) (Cell White)) = True
fair_exercise _ = False

main = print(fair_exercise(my_solution fair_exercise))
