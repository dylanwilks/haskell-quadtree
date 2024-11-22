{-# LANGUAGE ScopedTypeVariables #-}
import Control.Exception --Both imports used for catching errors
import System.IO.Unsafe

--default features
data Color = White | Black 
  deriving (Eq, Show)

data QuadTree = Cell Color | Quadrants QuadTree QuadTree QuadTree QuadTree | Empty
  deriving (Eq, Show)

allBlack :: QuadTree 
allWhite :: QuadTree

clockwise :: QuadTree -> QuadTree -> QuadTree -> QuadTree -> QuadTree
anticlockwise :: QuadTree -> QuadTree -> QuadTree -> QuadTree -> QuadTree

allBlack = Cell Black
allWhite = Cell White

clockwise q1 q2 q3 q4 = Quadrants q1 q2 q3 q4
anticlockwise q1 q2 q3 q4 = Quadrants q1 q4 q3 q2

--target
my_solution :: (QuadTree -> Bool) -> QuadTree

--functions
init_empty :: [a] -> [a]
map_quadrant :: (QuadTree -> QuadTree) -> QuadTree -> Int -> QuadTree
get_subtree :: QuadTree -> [Int] -> QuadTree
halt_error :: (QuadTree -> Bool) -> QuadTree -> Bool
replace_subtree :: QuadTree -> [Int] -> QuadTree -> QuadTree
increment_index :: [Int] -> [Int]
cascade_index :: [Int] -> [Int]
rightmost_quadtree :: QuadTree -> [Int]
check_left :: QuadTree -> [Int] -> Bool
place_quadrant :: QuadTree -> Int -> QuadTree -> QuadTree
check_combinations :: (QuadTree -> Bool) -> QuadTree -> [Int] -> [QuadTree] -> QuadTree
quadtree_blueprint :: (QuadTree -> Bool) -> QuadTree -> [Int] -> QuadTree

--implementations
init_empty [] = []
init_empty xs = init xs

map_quadrant f (Quadrants q1 q2 q3 q4) 0 = Quadrants (f q1) q2 q3 q4
map_quadrant f (Quadrants q1 q2 q3 q4) 1 = Quadrants q1 (f q2) q3 q4
map_quadrant f (Quadrants q1 q2 q3 q4) 2 = Quadrants q1 q2 (f q3) q4
map_quadrant f (Quadrants q1 q2 q3 q4) 3 = Quadrants q1 q2 q3 (f q4)
map_quadrant f q _ = f q

get_subtree q [] = q
get_subtree (Quadrants q1 q2 q3 q4) (0:xs) = get_subtree q1 xs
get_subtree (Quadrants q1 q2 q3 q4) (1:xs) = get_subtree q2 xs
get_subtree (Quadrants q1 q2 q3 q4) (2:xs) = get_subtree q3 xs
get_subtree (Quadrants q1 q2 q3 q4) (3:xs) = get_subtree q4 xs

halt_error f q = 
   unsafePerformIO (evaluate (if (f q == False) || (f q == True)
                              then False
                              else False)
                    `catch` \(_ :: ErrorCall) -> pure True)

replace_subtree _ [] q = q
replace_subtree p (x:xs) q = map_quadrant (\(r :: QuadTree) -> replace_subtree r xs q) p x

increment_index xs = reverse (cascade_index (reverse xs))
cascade_index [] = []
cascade_index (3:ys) = cascade_index ys
cascade_index (n:ys) = ((n+1):ys)

check_left q xs = 
    if last xs == 0 then False
    else if get_subtree q (prev_index ++ 
            (init_empty (rightmost_quadtree (get_subtree q prev_index)))) ==
            (Quadrants allWhite allWhite allWhite allWhite)
         then True
         else False
    where prev_index = ((init_empty xs) ++ [(last xs) - 1])

rightmost_quadtree (Quadrants q1 q2 q3 q4) =
    [3] ++ rightmost_quadtree q4
rightmost_quadtree _ = []

place_quadrant (Quadrants q1 q2 q3 q4) x q = 
    if x == 0 then (Quadrants q q2 q3 q4)
    else if x == 1 then (Quadrants q1 q q3 q4)
    else if x == 2 then (Quadrants q1 q2 q q4)
    else if x == 3 then (Quadrants q1 q2 q3 q)
    else (Quadrants q1 q2 q3 q4)

check_combinations f q xs [] = q
check_combinations f q xs (y:ys) =
    if halt_error f q then q
    else
        check_combinations
            f
            (replace_subtree q xs y)
            xs
            ys
          
quadtree_blueprint f q [] =
    if f q then q
    else
        let n1 = replace_subtree q (init (rightmost_quadtree q)) allWhite in
        let n2 = replace_subtree q (init (rightmost_quadtree q)) allBlack in
            if f n1 then n1 else n2

quadtree_blueprint f q xs = 
    (if halt_error f q then
        quadtree_blueprint 
            f 
            (replace_subtree q xs (Quadrants e allWhite allWhite allWhite))
            (xs ++ [0])
    else 
        if check_left q xs then 
            quadtree_blueprint 
                f 
                (check_combinations 
                    f 
                    q
                    (prev_index ++ (init_empty (rightmost_quadtree 
                        (get_subtree q prev_index))))
                    [allBlack, allWhite]
                )
                xs
        else
            quadtree_blueprint 
                f 
                (replace_subtree 
                    q 
                    (init_empty n_xs)
                    (place_quadrant
                        (get_subtree 
                            (replace_subtree
                                q
                                (init_empty xs)
                                (place_quadrant
                                    (get_subtree
                                        q
                                        (init_empty xs)
                                    )
                                    (last xs)
                                    allWhite
                                )
                            )
                            (init_empty n_xs)
                        ) 
                        (if n_xs == [] then 4
                         else last n_xs)
                        e
                    )
                )
                n_xs)
    where 
        e = (error "")
        n_xs = increment_index xs
        prev_index = ((init_empty xs) ++ [(last xs) - 1])

my_solution f = 
    quadtree_blueprint f (Quadrants (error "") allWhite allWhite allWhite) [0]

fair_exercise :: QuadTree -> Bool
fair_exercise (Quadrants (Cell White) (Quadrants (Cell White) (Cell White) (Cell Black) (Quadrants (Cell Black) (Cell Black) (Cell Black) (Cell Black))) (Cell Black) (Cell Black)) = True
fair_exercise _ = False

main = print(my_solution fair_exercise)
