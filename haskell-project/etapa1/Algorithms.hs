module Algorithms where

import qualified Data.Set as S
import qualified Data.List as L
import StandardGraph

{-
    În etapa 1, prin graf înțelegem un graf cu reprezentare standard.
    În etapele următoare, vom experimenta și cu altă reprezentare.

    type introduce un sinonim de tip, similar cu typedef din C.
-}
type Graph a = StandardGraph a

{-
    *** TODO ***

    Funcție generală care abstractizează BFS și DFS pornind dintr-un anumit nod,
    prin funcția de îmbinare a listelor care constituie primul parametru.
    
    Cele două liste primite ca parametru de funcția de îmbinare sunt lista
    elementelor deja aflate în structură (coadă/stivă), respectiv lista
    vecinilor nodului curent, proaspăt expandați.

    Căutarea ar trebui să țină cont de eventualele cicluri.

    Hint: Scrieți o funcție auxiliară care primește ca parametru suplimentar
    o mulțime (set) care reține nodurile vizitate până în momentul curent.
-}

-- removeDuplicates :: Ord a => [a] -> [a]
-- removeDuplicates list
--     | null list = list
--     | otherwise = (head list) : removeDuplicates (filter (\y -> y /= (head list)) (tail list))

getNeighbs :: Ord a => a -> StandardGraph a -> [a]
getNeighbs node graph = [snd x | x <- S.toList (edges graph), (fst x) == node]

searchAux :: Ord a
       => ([a] -> [a] -> [a])  
       -> a                    
       -> Graph a
       -> [a]
       -> [a]      
       -> [a]
searchAux f node graph list visited =
    if (length list) == 0 then []
    else
        let neighbours     = (getNeighbs node graph)
            newList        = L.nub (f (tail list) (filter (\x -> (not (elem x visited))) neighbours))
            updVisited     = node : visited
        in node : (searchAux f (head newList) graph newList updVisited)

search :: Ord a
       => ([a] -> [a] -> [a])  -- funcția de îmbinare a listelor de noduri
       -> a                    -- nodul de pornire
       -> Graph a              -- graful
       -> [a]                  -- lista obținută în urma parcurgerii
search f node graph = (searchAux f node graph [node] [])
    

{-
    *** TODO ***

    Strategia BFS, derivată prin aplicarea parțială a funcției search.

    Exemple:

    > bfs 1 graph4
    [1,2,3,4]

    > bfs 4 graph4
    [4,1,2,3]
-}
bfs :: Ord a => a -> Graph a -> [a]
bfs =
    let
        f = (\list neighs -> list ++ neighs)
    in (\node graph -> (search f node graph))

{-
    *** TODO ***

    Strategia DFS, derivată prin aplicarea parțială a funcției search.

    Exemple:

    > dfs 1 graph4 
    [1,2,4,3]
    
    > dfs 4 graph4
    [4,1,2,3]
-}
dfs :: Ord a => a -> Graph a -> [a]
dfs =
    let
        f = (\list neighs -> neighs ++ list)
    in (\node graph -> (search f node graph))

{-
    *** TODO ***

    Funcția numără câte noduri intermediare expandează strategiile BFS,
    respectiv DFS, în încercarea de găsire a unei căi între un nod sursă
    și unul destinație, ținând cont de posibilitatea absenței acesteia din graf.
    Numărul exclude nodurile sursă și destinație.

    Modalitatea uzuală în Haskell de a preciza că o funcție poate să nu fie
    definită pentru anumite valori ale parametrului este constructorul de tip
    Maybe. Astfel, dacă o cale există, funcția întoarce
    Just (numărBFS, numărDFS), iar altfel, Nothing.

    Hint: funcția span.

    Exemple:

    > countIntermediate 1 3 graph4
    Just (1,2)

    Aici, bfs din nodul 1 întoarce [1,2,3,4], deci există un singur nod
    intermediar (2) între 1 și 3. dfs întoarce [1,2,4,3], deci sunt două noduri
    intermediare (2, 4) între 1 și 3.

    > countIntermediate 3 1 graph4
    Nothing

    Aici nu există cale între 3 și 1.
-}
countIntermediate :: Ord a
                  => a                 -- nodul sursă
                  -> a                 -- nodul destinație
                  -> StandardGraph a   -- graful
                  -> Maybe (Int, Int)  -- numărul de noduri expandate de BFS/DFS
countIntermediate from to graph =
    let dfsSearch = (dfs from graph)
        bfsSearch = (bfs from graph)
        dfsSteps = (length $ fst (span (/=  to) dfsSearch)) - 1
        bfsSteps = (length $ fst (span (/=  to) bfsSearch)) - 1
    in if ((length dfsSearch == dfsSteps + 1) || (length bfsSearch == bfsSteps + 1) ||
            dfsSteps <= 0 || bfsSteps <= 0)
        then Nothing
       else Just (bfsSteps, dfsSteps)
    