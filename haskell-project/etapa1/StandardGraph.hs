{-# LANGUAGE TupleSections #-}
module StandardGraph where

import qualified Data.Set as S

{-
    Graf ORIENTAT cu noduri de tipul a, reprezentat prin mulțimile (set)
    de noduri și de arce.

    Mulțimile sunt utile pentru că gestionează duplicatele și permit
    testarea egalității a două grafuri fără a ține cont de ordinea nodurilor
    și a arcelor.

    type introduce un sinonim de tip, similar cu typedef din C.
-}
type StandardGraph a = (S.Set a, S.Set (a, a))

{-
    *** TODO ***

    Construiește un graf pe baza listelor de noduri și de arce.

    Hint: S.fromList.

    Constrângerea (Ord a) afirmă că valorile tipului a trebuie să fie
    ordonabile, lucru necesar pentru reprezentarea internă a mulțimilor.
    Este doar un detaliu, cu care nu veți opera explicit în această etapă.
    Veți întâlni această constrângere și în tipurile funcțiilor de mai jos.
-}
fromComponents :: Ord a
               => [a]              -- lista nodurilor
               -> [(a, a)]         -- lista arcelor
               -> StandardGraph a  -- graful construit
fromComponents ns es = (S.fromList ns, S.fromList es)

{-
    *** TODO ***

    Mulțimea nodurilor grafului.
-}
nodes :: StandardGraph a -> S.Set a
nodes = (\x -> (fst x))

{-
    *** TODO ***

    Mulțimea arcelor grafului.
-}
edges :: StandardGraph a -> S.Set (a, a)
edges = (\x -> (snd x))

{-
    Exemple de grafuri
-}
graph1 :: StandardGraph Int
graph1 = fromComponents [1, 2, 3, 3, 4] [(1, 2), (1, 3), (1, 2)]

graph2 :: StandardGraph Int
graph2 = fromComponents [4, 3, 3, 2, 1] [(1, 3), (1, 3), (1, 2)]

graph3 :: StandardGraph Int
graph3 = fromComponents [1, 2, 3, 4] [(1, 2), (1, 4), (4, 1), (2, 3), (1, 3)]

graph4 :: StandardGraph Int
graph4 = fromComponents [1, 2, 3, 4] [(1, 2), (1, 4), (4, 1), (2, 4), (1, 3)]

shouldBeTrue :: Bool
shouldBeTrue = graph1 == graph2

{-
    *** TODO ***

    Mulțimea nodurilor înspre care pleacă arce dinspre nodul curent.

    Exemplu:

    > outNeighbors 1 graph3
    fromList [2,3,4]
-}
outNeighbors :: Ord a => a -> StandardGraph a -> S.Set a
-- outNeighbors node graph = S.fromList [(snd n) | n <- (S.toList (edges graph)), (fst n) == node]
outNeighbors node graph = S.fromList [(snd n) | n <- S.toList $ S.filter (\x -> (fst x == node)) (edges graph)]

{-
    *** TODO ***

    Mulțimea nodurilor dinspre care pleacă arce înspre nodul curent.

    Exemplu:

    > inNeighbors 1 graph3 
    fromList [4]
-}
inNeighbors :: Ord a => a -> StandardGraph a -> S.Set a
inNeighbors node graph = S.fromList [(fst n) | n <- S.toList $ S.filter (\x -> (snd x == node)) (edges graph)]

{-
    *** TODO ***

    Întoarce graful rezultat prin eliminarea unui nod și a arcelor în care
    acesta este implicat. Dacă nodul nu există, întoarce același graf.

    Exemplu:

    > removeNode 1 graph3
    (fromList [2,3,4],fromList [(2,3)])
-}
removeNode :: Ord a => a -> StandardGraph a -> StandardGraph a
removeNode node graph = (gnodes, gedges)
    where
    gnodes = S.filter (\x -> x /= node) (nodes graph)
    gedges = S.filter (\x -> fst x /= node && snd x /= node) (edges graph)

{-
    *** TODO ***

    Divizează un nod în mai multe noduri, cu eliminarea nodului inițial.
    Arcele în care era implicat vechiul nod trebuie să devină valabile
    pentru noile noduri.

    Exemplu:

    > splitNode 2 [5,6] graph3
    (fromList [1,3,4,5,6],fromList [(1,3),(1,4),(1,5),(1,6),(4,1),(5,3),(6,3)])
-}
splitNode :: Ord a
          => a                -- nodul divizat
          -> [a]              -- nodurile cu care este înlocuit
          -> StandardGraph a  -- graful existent
          -> StandardGraph a  -- graful obținut
splitNode old news graph =
    let oldEdges            = (edges graph)
        edgesFromOld        = S.toList $ S.filter (\x -> fst x == old) oldEdges
        edgesToOld          = S.toList $ S.filter (\x -> snd x == old) oldEdges
        deletedOldGraph     = (removeNode old graph)
        addedEdges          = [(fst x, y) | x <- edgesToOld, y <- news] ++ [(x, snd y) | x <- news, y <- edgesFromOld]
        newNodes            = S.fromList (news ++ S.toList (nodes deletedOldGraph))
        newEdges            = S.fromList (addedEdges ++ S.toList (edges deletedOldGraph))
    in (newNodes, newEdges)


{-
    *** TODO ***

    Îmbină mai multe noduri într-unul singur, pe baza unei proprietăți
    respectate de nodurile îmbinate, cu eliminarea acestora. Arcele în care
    erau implicate vechile noduri vor referi nodul nou.

    Exemplu:

    > mergeNodes even 5 graph3
    (fromList [1,3,5],fromList [(1,3),(1,5),(5,1),(5,3)])
-}
mergeNodes :: Ord a
           => (a -> Bool)      -- proprietatea îndeplinită de nodurile îmbinate
           -> a                -- noul nod
           -> StandardGraph a  -- graful existent
           -> StandardGraph a  -- graful obținut
mergeNodes prop node graph =
    let oldEdges            = (edges graph)
        oldNodes            = (nodes graph)
        mergedNodes         = S.toList $ S.filter prop oldNodes
        edgesFromMerged     = S.toList $ S.filter (\x -> (elem (fst x) mergedNodes)) oldEdges
        edgesToMerged       = S.toList $ S.filter (\x -> (elem (snd x) mergedNodes)) oldEdges
        deletedMergedGraph  = foldl (\acc x -> (removeNode x acc)) graph mergedNodes

        mrgToMrgEdge        = [x | x <- edgesFromMerged, (elem (snd x) mergedNodes)] ++
                              [x | x <- edgesToMerged, (elem (fst x) mergedNodes)]

        addedEdges          = [(fst x, node) | x <- edgesToMerged, not (elem (fst x) mergedNodes)] ++
                              [(node, snd y) | y <- edgesFromMerged, not (elem (snd y) mergedNodes)]
        auxAddedEdges       = addedEdges ++ S.toList (edges deletedMergedGraph)

        newNodes            = if (nodes deletedMergedGraph == nodes graph)
                                then oldNodes
                              else S.fromList (node : S.toList (nodes deletedMergedGraph))
        newEdges            = if (length mrgToMrgEdge) > 0
                                then S.fromList ((node, node) : auxAddedEdges)
                              else S.fromList (auxAddedEdges)
    in (newNodes, newEdges)
