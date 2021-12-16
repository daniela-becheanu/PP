{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState
import qualified Data.PSQueue as PQ
import Data.Maybe
import Prelude
import qualified Data.Set as S

{-
    *** TODO ***
    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:
    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime;
    * estimarea costului până la starea finală;
    * copiii, ce vor desemna stările învecinate;
-}

data Node s a = Node 
    { state :: s
    , action :: Maybe a
    , parent :: Maybe (Node s a)
    , depth :: Int
    , cost :: Float
    , children :: [Node s a]
    } | NodeNil deriving Show

{-
    *** TODO ***
    Instanțiați Eq și Ord pe baza stării.
-}

instance Eq s => Eq (Node s a) where
    Node s1 _ _ _ _ _ == Node s2 _ _ _ _ _ = s1 == s2
    n1 /= n2 = not (n1 == n2)

instance Ord s => Ord (Node s a) where
    Node s1 _ _ _ _ _ <= Node s2 _ _ _ _ _ = s1 <= s2

{-
    *** TODO ***
    Gettere folosite pentru accesul la câmpurile nodului
-}

nodeState :: Node s a -> s
nodeState node = (state node)

nodeParent :: Node s a -> Maybe (Node s a)
nodeParent node = (parent node)

nodeDepth :: Node s a -> Int
nodeDepth node = (depth node)

nodeChildren :: Node s a -> [Node s a]
nodeChildren node = (children node)

nodeHeuristic :: Node s a -> Float
nodeHeuristic node = (cost node)

nodeAction :: Node s a -> Maybe a
nodeAction node = (action node)

{-
    *** TODO ***
    Generarea întregului spațiu al stărilor.
    Primește starea inițială și creează nodul corespunzător acestei stări,
    având drept copii nodurile succesorilor stării curente, și așa mai
    departe, recursiv.
-}

createStateSpace :: (ProblemState s a, Eq s) => s -> Node s a
createStateSpace initialState = createStateHelper initialState Nothing Nothing 0 (h initialState)

createStateHelper st action parent dep cost =
    let curr_node = Node st
                         action
                         parent
                         dep
                         cost
                         $ map (\x@(xx, xy) -> createStateHelper xy (Just xx) (Just curr_node) (dep + 1) (h xy)) (successors st)
    in curr_node
{-
    Funcție ce primește o coadă de priorități și întoarce o pereche
    formată din cheia cu prioritatea minimă și coada din care a fost ștearsă
    aceasta.
    Hint: O puteți folosi pentru a extrage și a șterge un nod din frontieră.
-}

deleteFindMin :: (Ord k, Ord p) => (PQ.PSQ k p) -> (k, (PQ.PSQ k p))
deleteFindMin pq = (minK, pq')
    where minK = PQ.key $ fromJust $ PQ.findMin pq
          pq' = PQ.deleteMin pq

{-
    *** TODO ***
    Primește nodul curent și mulțimea stărilor vizitate și întoarce
    o listă cu nodurile succesor nevizitate, care ar putea fi introduse
    în frontieră.
-}

suitableSuccs :: (ProblemState s a, Ord s) => Node s a -> (S.Set s) -> [Node s a]
suitableSuccs node visited = [n | n <- (children node), not ((state n) `elem` visited)]

{-
    *** TODO ***
    Primește o frontieră (o coadă de priorități) și un nod ce trebuie inserat în aceasta,
    întorcând o nouă frontieră.
    ATENȚIE: Dacă la introducerea unui nod există deja în frontieră un alt nod cu aceeași
    stare, dar cu cost mai mare, nodul nou, cu cost mai mic îl va înlocui pe cel vechi.
    
    Hints:
    1. Vedeți funcția insertWith din pachetul PSQueue.
        (https://hackage.haskell.org/package/PSQueue-1.1.0.1/docs/Data-PSQueue.html#v:insertWith)
    2. Costul se calculează ca suma dintre adâncime și euristică.
-}

insertSucc :: (ProblemState s a, Ord s) => (PQ.PSQ (Node s a) Float) -> Node s a -> PQ.PSQ (Node s a) Float
insertSucc frontier node = 
    let curr_cost = (fromIntegral (depth node)) + (h (state node))
        modif = node {cost = curr_cost}
        new_frontier | (PQ.lookup node frontier) == Nothing = (PQ.insert node curr_cost frontier)
                     | curr_cost < (fromJust (PQ.lookup node frontier)) = (PQ.insert modif curr_cost frontier)
                     | otherwise = frontier
    in new_frontier

{-
    *** TODO ***
    Primește nodul curent, frontiera și mulțimea stărilor vizitate, întorcând noua
    frontieră (coadă de priorități) în care au fost adăugate nodurile succesor validate
    de suitableSuccs.
-}

insertSuccs :: (ProblemState s a, Ord s) => (Node s a) -> (PQ.PSQ (Node s a) Float) -> (S.Set s) -> (PQ.PSQ (Node s a) Float)
insertSuccs node frontier visited = 
    let suitable_succs = suitableSuccs node visited
        new_frontier = (foldr (\x acc -> (insertSucc acc x)) frontier suitable_succs)
    in new_frontier

{-
    *** TODO ***
    Funcție helper care implementează A-star.
    Primește o mulțime de noduri vizitate și o coadă de priorități (aka frontiera) și
    întoarce starea finală.
    Se procedează astfel până la întâlnirea unei stări scop:
        - se extrage un nod adecvat din frontireră
        - se marchează starea acestuia ca fiind vizitată
        - se introduc succesorii în frontieră
-}

astar' :: (ProblemState s a, Ord s) => (S.Set s) -> (PQ.PSQ (Node s a) Float) -> Node s a
astar' visited frontier
    | (not (isGoal (nodeState (fst (deleteFindMin frontier))))) = astar' (S.insert (nodeState (fst (deleteFindMin frontier))) visited)
                                                                         (insertSuccs (fst (deleteFindMin frontier))
                                                                                      (snd (deleteFindMin frontier))
                                                                                      (S.insert (nodeState (fst (deleteFindMin frontier))) visited))
    | otherwise = (fst (deleteFindMin frontier))

{-
    *** TODO ***
  
    Primește starea inițială și întoarce starea finală pentru o singură aplicare
    a algoritmului.
    Asigură parametrii inițiali corecți pentru aplicarea funcției astar'.
-}

astar :: (ProblemState s a, Ord s) => Node s a -> Node s a
astar initialNode =
    let set = S.empty
        frontier = (insertSucc PQ.empty initialNode)
    in astar' set frontier

{-
    *** TODO ***
    Pornind de la un nod, reface parțial calea către nodul inițial, urmând legăturile
    către părinți.
    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea următoare
    stării inițiale și se încheie la starea finală.
    ATENȚIE: Nodul inițial este singurul exclus!
-}

extractPathHelper :: Node s a -> [(a, s)] -> [(a, s)]
extractPathHelper curr_node l
    | isJust (parent curr_node) = extractPathHelper (fromJust (parent curr_node)) l ++ [(fromJust (action curr_node), (state curr_node))]
    | otherwise = l

extractPath :: Node s a -> [(a, s)]
extractPath goalNode = extractPathHelper goalNode [] 