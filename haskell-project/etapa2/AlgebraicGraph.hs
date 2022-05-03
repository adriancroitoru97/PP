module AlgebraicGraph where

import qualified Data.Set as S

data AlgebraicGraph a
    = Empty
    | Node a
    | Overlay (AlgebraicGraph a) (AlgebraicGraph a)
    | Connect (AlgebraicGraph a) (AlgebraicGraph a)
    deriving (Eq, Show)

-- (1, 2), (1, 3)
angle :: AlgebraicGraph Int
angle = Connect (Node 1) (Overlay (Node 2) (Node 3))

-- (1, 2), (1, 3), (2, 3)
triangle :: AlgebraicGraph Int
triangle = Connect (Node 1) (Connect (Node 2) (Node 3))

{-
    *** TODO ***

    Mulțimea nodurilor grafului.

    Hint: S.union
-}
nodes :: Ord a => AlgebraicGraph a -> S.Set a
nodes Empty = S.empty
nodes (Node a) = S.fromList [a]
nodes (Overlay a b) = S.union (nodes a) (nodes b)
nodes (Connect a b) = S.union (nodes a) (nodes b)

{-
    *** TODO ***

    Mulțimea arcelor grafului.

    Hint: S.union, S.cartesianProduct
-}
edges :: Ord a => AlgebraicGraph a -> S.Set (a, a)
edges Empty = S.empty
edges (Node a) = S.empty
edges (Overlay a b) = S.union (edges a) (edges b)
edges (Connect a b) = S.union (S.union (S.cartesianProduct (nodes a) (nodes b)) (edges a)) (edges b)

{-
    *** TODO ***

    Mulțimea nodurilor înspre care pleacă arce dinspre nodul curent.

    ATENȚIE! NU folosiți funcția edges definită mai sus, pentru că ar genera
    prea multe muchii inutile.
-}
outNeighbors :: Ord a => a -> AlgebraicGraph a -> S.Set a
outNeighbors node (Overlay a b) = S.union (outNeighbors node a) (outNeighbors node b)
outNeighbors node (Connect a b) =
    let recUnion = S.union (outNeighbors node a) (outNeighbors node b)
    in if node `elem` S.toList (nodes a)
        then S.union (nodes b) recUnion
        else recUnion
outNeighbors node _ = S.empty

{-
    *** TODO ***

    Mulțimea nodurilor dinspre care pleacă arce înspre nodul curent.

    ATENȚIE! NU folosiți funcția edges definită mai sus, pentru că ar genera
    prea multe muchii inutile.
-}
inNeighbors :: Ord a => a -> AlgebraicGraph a -> S.Set a
inNeighbors node (Overlay a b) = S.union (inNeighbors node a) (inNeighbors node b)
inNeighbors node (Connect a b) =
    if node `elem` S.toList (nodes b)
        then S.union (nodes a) recUnion
    else recUnion
    where recUnion = S.union (inNeighbors node a) (inNeighbors node b)
inNeighbors node _ = S.empty

{-
    *** TODO ***

    Întoarce graful rezultat prin eliminarea unui nod și a arcelor în care
    acesta este implicat. Dacă nodul nu există, se întoarce același graf.

    Hint: Definiți o funcție recursivă locală (de exemplu, în where),
    care să primească drept parametri doar entități variabile de la un apel
    recursiv la altul, astfel încât să nu copiați la fiecare apel parametrii
    nemodificați. De exemplu, parametrul node nu se modifică, în timp ce
    parametrul graph se modifică.
-}
removeNode :: (Eq a) => a -> AlgebraicGraph a -> AlgebraicGraph a
removeNode node Empty = Empty
removeNode node (Node a) = if node == a then Empty else Node a
removeNode node (Overlay a b) = Overlay (func a) (func b)
        where func = removeNode node
removeNode node (Connect a b) = Connect (func a) (func b)
        where func = removeNode node

{-
    *** TODO ***

    Divizează un nod în mai multe noduri, cu eliminarea nodului inițial.
    Arcele în care era implicat vechiul nod trebuie să devină valabile
    pentru noile noduri.
    
    Hint: Funcție recursivă locală, ca la removeNode.
-}
makeGraphFromList :: [a] -> AlgebraicGraph a
makeGraphFromList list = if null list then Empty else Overlay (Node (head list)) (makeGraphFromList (tail list))

splitNode :: Eq a
          => a                 -- nodul divizat
          -> [a]               -- nodurile cu care este înlocuit
          -> AlgebraicGraph a  -- graful existent
          -> AlgebraicGraph a  -- graful obținut
splitNode old news Empty = Empty
splitNode old news (Node a) = if old == a then makeGraphFromList news else Node a
splitNode old news (Overlay a b) = Overlay (func a) (func b)
        where func = splitNode old news
splitNode old news (Connect a b) = Connect (func a) (func b)
        where func = splitNode old news

{-
    *** TODO ***

    Îmbină mai multe noduri într-unul singur, pe baza unei proprietăți
    respectate de nodurile îmbinate, cu eliminarea acestora. Arcele în care
    erau implicate vechile noduri vor referi nodul nou.

    Hint: Funcție recursivă locală, ca la removeNode.
-}
mergeNodes :: (a -> Bool)       -- proprietatea îndeplinită de nodurile îmbinate
           -> a                 -- noul nod
           -> AlgebraicGraph a  -- graful existent
           -> AlgebraicGraph a  -- graful obținut
mergeNodes prop node Empty = Empty
mergeNodes prop node (Node a) = if prop a then Node node else Node a
mergeNodes prop node (Overlay a b) = Overlay (func a) (func b)
        where func = mergeNodes prop node
mergeNodes prop node (Connect a b) = Connect (func a) (func b)
        where func = mergeNodes prop node
