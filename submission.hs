--For debugging
import Debug.Trace
debug = flip trace

--Exercise 1 : Quadtrees

--Algebraic data types--

  --color to correctly compare cells
data Color = White | Black 
  deriving (Eq, Show)

  --quadtree recursive data constructors; Cell represents the base case of 1 color
data QuadTree = Cell Color Int | Quadrants QuadTree QuadTree QuadTree QuadTree 
  deriving (Eq, Show)


--Type constructors--

  --for constructing cells of a certain color and given size;
  --Int represents the size and QuadTree is the Cell
allBlack :: Int -> QuadTree 
allWhite :: Int -> QuadTree

  --for checking arrangement of quadtrees;
  --takes four QuadTree types as input and returns a QuadTree parent node of the given inputs
clockwise :: QuadTree -> QuadTree -> QuadTree -> QuadTree -> QuadTree
anticlockwise :: QuadTree -> QuadTree -> QuadTree -> QuadTree -> QuadTree

  --supplementary functions for processing dimensions
size :: QuadTree -> Int  
checkEq :: Int -> Int -> Int -> Int -> Bool


--Function definitions--

  --return QuadTree type with Cell data constructor of corresponding color and size n
allBlack n = Cell Black n
allWhite n = Cell White n

  --returns the dimension of the quadtree
size (Cell _ n) = n * n --square dimensions
size (Quadrants q1 q2 q3 q4) = (size q1) + (size q2) + (size q3) + (size q4) --number of cells in
                                                                             --parent node is sum of child cells
  --check if cell dimensions of each quadrant match                                               
checkEq i1 i2 i3 i4 = (i1 == i2) &&                                          
                      (i1 == i3) && 
                      (i1 == i4) &&
                      (i2 == i3) &&
                      (i2 == i4) &&
                      (i3 == i4)


  --use Quadrants data constructor to return QuadTree parent node of 4 inputs.
  --first checks if dimensions are correct. If so return a quadtree, else an error.
clockwise q1 q2 q3 q4 = if (checkEq (size q1) (size q2) (size q3) (size q4)) 
                        then Quadrants q1 q2 q3 q4
                        else error "ERROR::INVALID_DIMENSIONS"

anticlockwise q1 q2 q3 q4 = if (checkEq (size q1) (size q2) (size q3) (size q4)) 
                            then Quadrants q1 q4 q3 q2
                            else error "ERROR::INVALID_DIMENSIONS"


--Exercise 2 : Blur                                                      ---------  
--                                                                       | 1 | 2 |  <- Visual used to
--Type Constructors--                                                    |---|---|     determine quadrant
--                                                                       | 4 | 3 |     values
  --main blurring function                                               ---------
blur :: QuadTree -> QuadTree

  --blur2 function to take over the job of blur completely to 
  --allow coordinates and parent nodes to be passed down
blur2 :: QuadTree -> [QuadTree] -> [Int] -> QuadTree

  --functions used for purely pattern matching when checking value of returned QuadTree types.
  --getQuadrant: returns the corresponding quadrant of the given 'Quadrants' node based on Int
  --checkCell: returns True if input is a Cell or Nothing; False otherwise
getQuadrant :: Maybe QuadTree -> Int -> Maybe QuadTree
checkCell :: Maybe QuadTree -> Bool

  --adjacent functions that return the corresponding QuadTree of the same size
  --or possibly larger if the return value is a Cell. 
  --[Int] represents the node coordinates and [QuadTree] the parent nodes all the way to the root.
adjacentTop :: [QuadTree] -> [Int] -> Maybe QuadTree
adjacentRight :: [QuadTree] -> [Int] -> Maybe QuadTree
adjacentBottom :: [QuadTree] -> [Int] -> Maybe QuadTree       --'Maybe' is used as to yield Nothing
adjacentLeft :: [QuadTree] -> [Int] -> Maybe QuadTree         --when at a border

  --as the adjacent functions may return all 4 quadrants of a node it needs
  --to be reduced into the Cells that reside against an edge
reduce_top_quadtree :: Maybe QuadTree -> [QuadTree]
reduce_right_quadtree :: Maybe QuadTree -> [QuadTree]
reduce_bottom_quadtree :: Maybe QuadTree -> [QuadTree]
reduce_left_quadtree :: Maybe QuadTree -> [QuadTree]

  --to be used with filter on the reduce functions; input is a Cell
isBlack :: QuadTree -> Bool
isWhite :: QuadTree -> Bool

  --return the number of neighbors with corresponding color
  --based on given node coordinates ([Int]).
  --[QuadTree] input is used to pass parent nodes to the adjacent functions.
blackCount :: [QuadTree] -> [Int] -> Int
whiteCount :: [QuadTree] -> [Int] -> Int


--Function definitions--

  --simply returns the corresponding quadrant number
getQuadrant (Just (Quadrants q1 q2 q3 q4)) x
  | x == 1 = Just q1
  | x == 2 = Just q2
  | x == 3 = Just q3
  | x == 4 = Just q4
  |otherwise = error "ERROR::INVALID_QUADRANT_NUMBER"

getQuadrant (Just (Cell _ _)) _ = Nothing --otherwise Nothing if it is a cell

  --return True if QuadTree input is not a node with children
checkCell (Just (Cell _ _)) = True
checkCell Nothing = True
checkCell (Just (Quadrants _ _ _ _)) = False


  --Adjacent definitions--
  --
  --Note: comments are done only for the adjacentTop algorithm.
  --The rest function similarly; the only difference being what is returned
  --based on the coordinate value.
 
  --definitions for identifying the top adjacent quadtree and reducing it
adjacentTop ((Quadrants q1 q2 q3 q4):[]) (x:[])
  | x == 1 = Nothing
  | x == 2 = Nothing 
  | x == 3 = Just q2
  | x == 4 = Just q1
  | otherwise = error "ERROR::INVALID_QUADRANT_NUMBER"  --evaluating top adjacent node from the root

adjacentTop ((Quadrants q1 q2 q3 q4):qs) (x:xs) = 
  let node = if x == 1 then adjacentTop qs xs       --recursively check parent nodes if the top
             else if x == 2 then adjacentTop qs xs  --adjacent node is outside the current parent node.
             else if x == 3 then Just q2  --otherwise just directly return the 
             else if x == 4 then Just q1  --adjacent node in the current quadtree.
             else error "ERROR::INVALID_QUADRANT_NUMBER"

  in if checkCell node then node              --if the returned node is a (Just) Cell or Nothing then return it.
     else if x == 1 then getQuadrant node 4   --Otherwise as each 'step back' in the recursion means the
     else if x == 2 then getQuadrant node 3   --Cell evaluated is half the size of the parent node, we 
     else node                                --return one of the bottom quadrants of node as to half it
                                              --as well so they end up equal in sizes and remain adjacent.
reduce_top_quadtree Nothing = []
reduce_top_quadtree (Just (Cell White n)) = [Cell White n]  --base cases for when the QuadTree does not have
reduce_top_quadtree (Just (Cell Black n)) = [Cell Black n]  --any children
reduce_top_quadtree (Just (Quadrants q1 q2 q3 q4)) = 
  (reduce_top_quadtree (Just q3)) ++ (reduce_top_quadtree (Just q4)) --otherwise recursively check the bottom
                                                                     --2 QuadTrees and append the results
                                                                     --together

  --definitions for identifying the right adjacent quadtree and reducing it
adjacentRight ((Quadrants q1 q2 q3 q4):[]) (x:[])
  | x == 1 = Just q2
  | x == 2 = Nothing
  | x == 3 = Nothing
  | x == 4 = Just q3
  |otherwise = error "ERROR::INVALID_QUADRANT_NUMBER"

adjacentRight ((Quadrants q1 q2 q3 q4):qs) (x:xs) = 
  let node = if x == 1 then Just q2
             else if x == 2 then adjacentRight qs xs
             else if x == 3 then adjacentRight qs xs
             else if x == 4 then Just q3
             else error "ERROR::INVALID_QUADRANT_NUMBER"

  in if checkCell node then node
     else if x == 2 then getQuadrant node 1
     else if x == 3 then getQuadrant node 4
     else node 

reduce_right_quadtree Nothing = []
reduce_right_quadtree (Just (Cell White n)) = [Cell White n]
reduce_right_quadtree (Just (Cell Black n)) = [Cell Black n]
reduce_right_quadtree (Just (Quadrants q1 q2 q3 q4)) = 
  (reduce_right_quadtree (Just q1)) ++ (reduce_right_quadtree (Just q4))
  
  --definitions for identifying the bottom adjacent quadtree and reducing it
adjacentBottom ((Quadrants q1 q2 q3 q4):[]) (x:[])
  | x == 1 = Just q4
  | x == 2 = Just q3
  | x == 3 = Nothing 
  | x == 4 = Nothing 
  |otherwise = error "ERROR::INVALID_QUADRANT_NUMBER"

adjacentBottom ((Quadrants q1 q2 q3 q4):qs) (x:xs) = 
  let node = if x == 1 then Just q4 
             else if x == 2 then Just q3
             else if x == 3 then adjacentBottom qs xs 
             else if x == 4 then adjacentBottom qs xs 
             else error "ERROR::INVALID_QUADRANT_NUMBER"

  in if checkCell node then node
     else if x == 3 then getQuadrant node 2
     else if x == 4 then getQuadrant node 1
     else node 

reduce_bottom_quadtree Nothing = []
reduce_bottom_quadtree (Just (Cell White n)) = [Cell White n]
reduce_bottom_quadtree (Just (Cell Black n)) = [Cell Black n]
reduce_bottom_quadtree (Just (Quadrants q1 q2 q3 q4)) = 
  (reduce_bottom_quadtree (Just q1)) ++ (reduce_bottom_quadtree (Just q2))

  --definitions for identifying the left adjacent quadtree and reducing it
adjacentLeft ((Quadrants q1 q2 q3 q4):[]) (x:[])
  | x == 1 = Nothing
  | x == 2 = Just q1
  | x == 3 = Just q4
  | x == 4 = Nothing
  | otherwise = error "ERROR::INVALID_QUADRANT_NUMBER"

adjacentLeft ((Quadrants q1 q2 q3 q4):qs) (x:xs) = 
  let node = if x == 1 then adjacentLeft qs xs
             else if x == 2 then Just q1
             else if x == 3 then Just q4
             else if x == 4 then adjacentLeft qs xs
             else error "ERROR::INVALID_QUADRANT_NUMBER"

  in if checkCell node then node
     else if x == 1 then getQuadrant node 2
     else if x == 4 then getQuadrant node 3
     else node 

reduce_left_quadtree Nothing = []
reduce_left_quadtree (Just (Cell White n)) = [Cell White n]
reduce_left_quadtree (Just (Cell Black n)) = [Cell Black n]
reduce_left_quadtree (Just (Quadrants q1 q2 q3 q4)) = 
  (reduce_left_quadtree (Just q2)) ++ (reduce_left_quadtree (Just q3))


isBlack (Cell Black _) = True
isBlack (Cell White _) = False

isWhite (Cell Black _) = False
isWhite (Cell White _) = True


blackCount qs xs = 
  length(filter isBlack (reduce_top_quadtree(adjacentTop qs xs) ++        --append the reduced trees of all
                         reduce_right_quadtree(adjacentRight qs xs) ++    --adjacent nodes together, then
                         reduce_bottom_quadtree(adjacentBottom qs xs) ++  --filter out all the white cells
                         reduce_left_quadtree(adjacentLeft qs xs)))       --and return the length of the list.

whiteCount qs xs = 
  length(filter isWhite (reduce_top_quadtree(adjacentTop qs xs) ++       --similar but with black cells instead
                         reduce_right_quadtree(adjacentRight qs xs) ++
                         reduce_bottom_quadtree(adjacentBottom qs xs) ++
                         reduce_left_quadtree(adjacentLeft qs xs)))
     
blur2 (Quadrants q1 q2 q3 q4) qs xs = Quadrants (blur2 q1 (q:qs) (1:xs))  --recursively call blur2 on each
                                                (blur2 q2 (q:qs) (2:xs))  --quadrant until we hit a cell;
                                                (blur2 q3 (q:qs) (3:xs))  --pass down coordinates and parent
                                                (blur2 q4 (q:qs) (4:xs))  --nodes as we go along
  --`debug` (show (q:qs))
                                      where q = Quadrants q1 q2 q3 q4

blur2 (Cell Black n) qs xs = (if (whiteCount qs xs) > (blackCount qs xs)  --checking if there is more of
                             then Cell White n                            --1 color over the other
                             else Cell Black n)
  --`debug` ("COORDS: " ++ show xs ++ " | WHITE_COUNT;BLACK_COUNT: "
  --                               ++ show (whiteCount qs xs)
  --                               ++ "," ++ show (blackCount qs xs))

blur2 (Cell White n) qs xs = (if (blackCount qs xs) > (whiteCount qs xs)
                             then Cell Black n
                             else Cell White n)
  --`debug` ("COORDS: " ++ show xs ++ " | WHITE_COUNT;BLACK_COUNT: "
  --                               ++ show (whiteCount qs xs)
  --                               ++ "," ++ show (blackCount qs xs))

blur (Quadrants q1 q2 q3 q4) = blur2 (Quadrants q1 q2 q3 q4) [] []
blur (Cell White n) = Cell White n
blur (Cell Black n) = Cell Black n
