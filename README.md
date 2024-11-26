# haskell-quadtree

Solutions for the problems regarding quadtrees in haskell offered by the University of Manchester.\
The 3 problems can be found in `lab.pdf`.
 - `submission.hs`: This solves the 1st and 2nd problems. The 1st problem is about encoding a black and white quadtree in haskell, and the 2nd problem
is about implementing a (crude) blur functionality for these quadtrees. The approach taken will not be mentioned here but is described in the comments in the file.
 - `creative.hs`: This solves the 3rd problem. This is described below.
 - `optimized.hs`: This solves a special case of the 3rd problem in a short amount of time. It requires the fair exercise (see below) to pattern match only 1
quadtree that does not contain any wild cards, e.g.
```haskell
fair_exercise :: QuadTree -> Bool
fair_exercise (Quadrants (Cell White) (Cell Black) (Cell White) (Cell Black)) = True
fair_exercise _ = False
```

The main problem of interest is the 3rd one. A summary of it is as follows:\
Suppose we have a function $f : Q \rightarrow \mathbb{B}$ that maps the set of both finite and infinite ergodomestic (see below) quadtrees to $\lbrace \top, \bot \rbrace$.

<p align="center">
  <img src=https://github.com/user-attachments/assets/846d4839-9227-440c-abd9-96408b4c3c3b width="600">
</p>

We say a quadtree $q$ is ergodomestic if the following function returns successfully for all $n \in \mathbb{N}$ given input $q$:
```haskell
coarsework :: Int -> QuadTree -> QuadTree
coarsework 0 _ = Cell White
coarsework n (Cell White) = Cell White
coarsework n (Cell Black) = Cell Black
coarsework (n+1) (Quadrants q1 q2 q3 q4)
                 = Quadrants (coarsework n q1)
                             (coarsework n q2)
                             (coarsework n q3)
                             (coarsework n q4)
```

In other words, an ergodomestic quadtree only comprises of values `Cell White`, `Cell Black`, and `Quadrants`: it cannot contain a value that, say, encodes an infinite subtree.
From now on it will be implicity assumed that every quadtree mentioned are ergodomestic unless specified otherwise.

We say $f$ is a _fair exercise_ $\Leftrightarrow$ it halts in finite time for all $q \in Q$. And with this we can now describe the problem:

__Define a function `my_solution` where, given an arbitrary fair exercise `f`, it returns another quadtree s.t.__
 - __`f (my_solution f) == True` if there exists a quadtree `q` s.t. `f q == True`__
 - __`f (my_solution f) == False` otherwise.__

This problem seems similar to the conundrum that is the halting problem in the case of `f q == False` for all quadtrees `q`. Suprisingly it can be solved by abusing errors.

The key is to recognize that fair exercises must halt in finite time: they can only pattern match up to a certain depth, as otherwise it would go on indefinitely and never halt. 
Note that this also means if the fair exercise returns `True` or `False` for an infinite quadtree, it must return the same value for a finite quadtree.
So to approach this for a given fair exercise $f$ we iterate over every quadtree of maximum depth $n$ for all $n \in \mathbb{N}$ (in that order) and test each one using a function called `halt_error`, 
which is the crux of the algorithm.\
It returns `True` if an error is matched, `False` otherwise.
```haskell
halt_error :: (QuadTree -> Bool) -> QuadTree -> Bool
halt_error f q =
    unsafePerformIO (evaluate (if (f q == False) || (f q == True)
                               then False else False)
                    `catch` \(_ :: ErrorCall) -> pure True)

```
The only difference is that each node of depth $n$ in these quadtrees is replaced with `(error "")`.

<p align="center">
  <img src="https://github.com/user-attachments/assets/72d11d8a-f647-4878-8dfa-c72811a11da1" width="500">
</p>

We iterate over every one of these quadtrees because - thanks to haskell's lazy evaluation - the fair exercise immediately returns `False` if it mismatches one of the nodes of lesser depth in the quadtree,
and would hence not match the errors.

The idea behind doing this is, if we yield an error in our search of quadtrees of depth $n$, and then yield no errors for quadtrees of depth $(n+1)$, then $f$ only pattern matches quadtrees
up to depth $n$. All that remains to do is to test $f$ on all quadtrees with $n \leq (n+1)$.

The solution is described in more detail in `creative.hs` through the comments. The first half of the code is concerned about enumerating both finite and infinite quadtrees. It does this by constructing
every possible cyclic multi-digraph where all but 2 nodes (the white and black nodes) have 4 outgoing edges to other nodes, possibly including itself. Each graph is then converted to its corresponding quadtree.
The second half does what was just discussed, using the aforementioned graph method with $n$ nodes. If none of these quadtrees return `True` for a given fair exercise, then we return `False`.
