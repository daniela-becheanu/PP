{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}

module Basics where
{-
    Expune funcțiile necesare reprezentării jocului.
-}

import ProblemState
import Data.List
import Data.Maybe

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc.
    Colțul stânga-sus este (0, 0).
-}
type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea Target-urilor.
    Acestea conțin informații atât despre poziția curentă a
    Target-ului cât și despre comportamentul acestuia.
    Tipul Behavior este definit mai jos.
-}
data Target = Target {
    position :: Position,
    behavior :: Behavior
}

instance Eq Target where
    Target p1 _ == Target p2 _ = p1 == p2

instance Ord Target where
    Target p1 _ <= Target p2 _ = p1 <= p2

{-
    Tip de date pentru reprezentarea comportamentului unui Target.
    Tipul Behavior este utilizat pentru a modela tranziția Target-urilor
    din starea curentă în starea următoare. Primul parametru este poziția
    actuală a target-ului, iar al doilea, starea curentă a jocului.
    Tipul Game este definit mai jos.
    
    Observați că, din moment ce un Behavior produce un Target nou,
    acesta din urmă ar putea fi caracterizat de un alt Behavior
    decât cel anterior.
-}
type Behavior = Position -> Game -> Target

{-
    Direcțiile de deplasare pe tablă
-}
data Direction = North | South | West | East
    deriving (Eq, Show)

{-
    *** TODO ***
    
    Tip de date pentru reprezentarea stării jocului, la un anumit
    moment. Completați-l cu orice informație aveți nevoie pentru
    stocarea stării jocului (hunter, target, obstacole, gateways).
-}

data Game = Game
    { hunter :: Position
    , targets :: [Target]
    , gateways :: [(Position, Position)]
    , obstacles :: [Position]
    , line_no :: Int
    , col_no :: Int
} deriving (Eq, Ord)
{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Game.
    În cazul acesta, eliminați deriving (Eq, Ord) din Game.
-}

{-
    *** TODO ***

    Reprezentați starea jocului ca șir de caractere, pentru afișarea
    la consolă.
    
    Atenție! Fiecare linie, mai puțin ultima, este urmată de \n.
    Celule goale vor fi reprezentate ca ' '.
    Hunter-ul va fi reprezentat ca '!'.
    Target-urile vor fi reprezentate ca '*'
    Gateways-urile vor fi reprezentate ca '#'.
    Obstacolele vor fi reprezentate de '@'.

    Hint: S-ar putea să vă fie utile list comprehensions,
    precum și funcțiile elem, any și intercalate din Data.List.
-}

convert ::  Game -> Position -> Char
convert game pos
    | pos `elem` (map (position) (targets game)) = '*'
    | pos `elem` (obstacles game) = '@'
    | pos `elem` (map fst (gateways game)) = '#'
    | pos `elem` (map snd (gateways game)) = '#'
    | pos == (hunter game) = '!'
    | otherwise = ' '
{-
    modifica cu let!!!
-}
gameAsString :: Game -> String
gameAsString game = let my_list = map (\l -> map (convert game) l) [[(i,j) | j <- [0..((col_no game) - 1)]] | i <- [0..((line_no game) - 1)]]
                    in intercalate "\n" my_list

instance Show Game where
    show = gameAsString

{-
    *** TODO ***
    
    Primește numărul de linii și numărul de coloane ale tablei de joc.
    Intoarce un obiect de tip Game în care tabla conține spații goale în interior, fiind
    împrejmuită de obstacole pe toate laturile. Implicit, colțul din stânga sus este (0,0),
    iar Hunterul se găsește pe poziția (1, 1).
-}
emptyGame :: Int -> Int -> Game
emptyGame lin col = let obst = lins ++ columns
                        lins = (foldr (++) [] [[(i,j) | j <- [0, col - 1]] | i <- [0..(lin - 1)]])
                        columns = (foldr (++) [] [[(i,j) | i <- [0, lin - 1]] | j <- [0..(col - 1)]])
                    in (Game (1, 1) [] [] obst lin col)

{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, cu Hunter-ul pus
    pe poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugat Hunter-ul
    Daca poziția este invalidă (ocupată sau în afara tablei de joc) se va întoarce
    același joc.
-}

validPos :: Position -> Game -> Bool
validPos pos game
    | (fst pos) >= (line_no game) = False
    | (snd pos) >= (col_no game) = False
    | (fst pos) < 0 = False
    | (snd pos) < 0 = False
    | otherwise = True

addHunter :: Position -> Game -> Game
addHunter pos game
    | (validPos pos game) == False = game
    | (convert game pos) == ' ' = game {hunter = pos}
    | otherwise = game

{-
    *** TODO ***

    Primește un comportament, o poziție și un joc și întoarce un nou joc, în care a fost
    adăugat Target-ul descris de comportament și poziție.
    Parametrul Behavior reprezintă comportamentul Hunter-ului care va fi adăugat.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat Target-ul.
-}
addTarget :: Behavior -> Position -> Game -> Game
addTarget b pos game
    | (validPos pos game) == False = game
    | (convert game pos) == ' ' = game { targets = (Target pos b) : (targets game) }
    | otherwise = game


{-
    *** TODO ***

    Primește o pereche de poziții și un joc și întoarce un nou joc, în care au fost adăugate
    cele două gateway-uri interconectate.
    Parametrul (Position, Position) reprezintă pozițiile de pe hartă la care vor fi adăugate 
    cele două gateway-uri interconectate printr-un canal bidirecțional.
-}
addGateway :: (Position, Position) -> Game -> Game
addGateway (pos1, pos2) game
    | ((validPos pos1 game) == False) || ((validPos pos2 game) == False) = game
    | (((convert game pos1) == ' ' || (convert game pos1) == '!') && ((convert game pos2) == ' ' || (convert game pos2) == '!')) = game { gateways = (pos1, pos2):(gateways game) }
    | otherwise = game

{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, în care a fost adăugat un obstacol
    la poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat obstacolul.
-}
addObstacle :: Position -> Game -> Game
addObstacle pos game
    | (validPos pos game) == False = game
    | (convert game pos) == ' ' = game { obstacles = pos : (obstacles game) }
    | otherwise = game

{-
    *** TODO ***
    
    Primește o poziție destinație înspre care vrea să se deplaseze o entitate (Hunter sau Target)
    și verifică daca deplasarea este posibilă, întorcând noua poziție, luând în considerare
    și Gateway-urile.
    Avem următoarele cazuri:
    - dacă poziția corespunde unui spațiu gol, se întoarce acea poziție;
    - dacă poziția corespunde unui gateway, se întoarce poziția gateway-ului pereche;
    - dacă poziția corespunde unui obstacol, se întoarce Nothing.
    Parametrul Position reprezintă poziția destinație.
-}
attemptMove :: Position -> Game -> Maybe Position
attemptMove pos game
    | (validPos pos game) == False = Nothing
    | (convert game pos) == ' ' = Just pos
    | (convert game pos) == '@' = Nothing
    | (pos `elem` (map fst (gateways game))) || (pos `elem` (map snd (gateways game))) =
                                  let l = [(x,y)| (x, y) <- (gateways game), x == pos || y == pos]
                                      first = (fst (head l))
                                      second = (snd (head l))
                                  in case () of
                                          ()| second == pos -> Just first
                                            | first == pos -> Just second
                                            | otherwise -> Just pos
                                
    | otherwise = Just pos

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre est. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
    
    Conform definiției, tipul Behavior corespunde tipului funcție
    Position -> Game -> Target.
    
    Având în vedere că cele patru funcții definite în continuare (goEast, goWest,
    goNorth, goSouth) sunt foarte similare, încercați să implementați o funcție
    mai generală, pe baza căreia să le definiți apoi pe acestea patru.
-}
goEast :: Behavior
goEast pos@(x,y) game
    | ((pos `elem` (map fst (gateways game))) || (pos `elem` (map snd (gateways game))))
      && isNothing (attemptMove (x, y + 1) game) = Target (fromJust (attemptMove (x, y) game)) goEast
    | isJust (attemptMove (x, y + 1) game) = Target (fromJust (attemptMove (x, y + 1) game)) goEast
    | otherwise = Target pos goEast

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre vest. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goWest :: Behavior
goWest pos@(x,y) game
    | ((pos `elem` (map fst (gateways game))) || (pos `elem` (map snd (gateways game))))
      && (isNothing (attemptMove (x, y - 1) game)) = Target (fromJust (attemptMove (x, y) game)) goWest
    | isJust (attemptMove (x, y - 1) game) = Target (fromJust (attemptMove (x, y - 1) game)) goWest
    | otherwise = Target pos goWest

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre nord. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goNorth :: Behavior
goNorth pos@(x,y) game
    | ((pos `elem` (map fst (gateways game))) || (pos `elem` (map snd (gateways game))))
      && (isNothing (attemptMove (x - 1, y) game)) = Target (fromJust (attemptMove (x, y) game)) goNorth
    | isJust (attemptMove (x - 1, y) game) = Target (fromJust (attemptMove (x - 1, y) game)) goNorth
    | otherwise = Target pos goNorth

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre sud. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goSouth :: Behavior
goSouth pos@(x,y) game
    | ((pos `elem` (map fst (gateways game))) || (pos `elem` (map snd (gateways game))))
      && (isNothing (attemptMove (x + 1, y) game)) = Target (fromJust (attemptMove (x, y) game)) goSouth
    | isJust (attemptMove (x + 1, y) game) = Target (fromJust (attemptMove (x + 1, y) game)) goSouth
    | otherwise = Target pos goSouth

{-
    *** TODO ***

    Comportamentul unui Target de a-și oscila mișcarea, când înspre nord, când înspre sud. 
    Mișcarea se poate face doar dacă poziția este validă (se află pe tablă de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul iși va schimba
    direcția de mers astfel:
    - daca mergea inspre nord, își va modifica direcția miscării înspre sud;
    - daca mergea inspre sud, își va continua mișcarea înspre nord.
    Daca Target-ul întâlneste un Gateway pe traseul său, va trece prin acesta,
    către Gateway-ul pereche conectat și își va continua mișcarea în același sens la ieșire
    din acesta.
    Puteți folosit parametrul Int pentru a surprinde deplasamentul Target-ului (de exemplu,
    1 pentru sud, -1 pentru nord).
-}
bounce :: Int -> Behavior
bounce depl pos@(x,y) game
    | isJust (attemptMove (x + depl, y) game) = Target (fromJust (attemptMove (x + depl, y) game)) (bounce depl)
    | otherwise = bounce (negate depl) pos game

{-
    *** TODO ***
    Funcție care mută toate Target-urile din Game-ul dat o poziție, în functie
    de behavior-ul fiecăreia și întoarce noul Game în care pozițiile Target-urilor
    sunt actualizate.
    
-}
moveTargets :: Game -> Game
moveTargets game = game {targets = (map (\t -> (behavior t) (position t) game) (targets game))}

{-
    *** TODO ***

    Verifică dacă Targetul va fi eliminat de Hunter.
    Un Target este eliminat de Hunter daca se află pe o poziție adiacentă
    cu acesta.
    Parametrul Position reprezintă poziția Hunterului pe tabla
    de joc.
    Parametrul Target reprezintă Targetul pentru care se face verificarea.
-}
isTargetKilled :: Position -> Target -> Bool
isTargetKilled h_pos@(hx, hy) target =
    let t_pos = (position target)
    in (hx == (fst t_pos) && ((hy == (snd t_pos - 1)) || (hy == (snd t_pos + 1)))) ||
       (hy == (snd t_pos) && ((hx == (fst t_pos - 1)) || (hx == (fst t_pos + 1)))) 


{-
    *** TODO ***

    Avansează starea jocului curent, rezultând starea următoare a jocului.
    Parametrul Direction reprezintă direcția în care se va deplasa Hunter-ul.
    Parametrul Bool specifică dacă, după mutarea Hunter-ului, vor fi
    mutate și Target-urile sau nu, și dacă vor fi eliminate din joc sau nu.
    Este folosit pentru a distinge între desfășurarea reală a jocului (True)
    și planificarea „imaginată” de hunter (False) în partea a doua a temei.

    Avansarea stării jocului respectă următoarea ordine:
    1. Se deplasează Hunter-ul.
    2. În funcție de parametrul Bool, se elimină Target-urile omorâte de către Hunter.
    3. In funcție de parametrul Bool, se deplasează Target-urile rămase pe tablă.
    4. Se elimină Targeturile omorâte de către Hunter și după deplasarea acestora.
    
    Dubla verificare a anihilării Target-urilor, în pașii 2 și 4, îi oferă Hunter-ului
    un avantaj în prinderea lor.
-}

moveHunter :: Direction -> Game -> Game
moveHunter dir game =
    let h_pos = (hunter game)
        x = (fst h_pos)
        y = (snd h_pos)
        modif | dir == East && isJust (attemptMove (x, y + 1) game) = fromJust (attemptMove (x, y + 1) game)
              | dir == West && isJust (attemptMove (x, y - 1) game) = fromJust (attemptMove (x, y - 1) game)
              | dir == South && isJust (attemptMove (x + 1, y) game) = fromJust (attemptMove (x + 1, y) game)
              | dir == North && isJust (attemptMove (x - 1, y) game) = fromJust (attemptMove (x - 1, y) game)
              | otherwise = h_pos
    in game {hunter = modif}



advanceGameState :: Direction -> Bool -> Game -> Game
advanceGameState dir bool game =
    let game1 = (moveHunter dir game)
        h_pos = (hunter game1)
        new_targets1 | bool == True = (filter (not . (isTargetKilled h_pos)) (targets game1))
                     | otherwise = (targets game1)
        game2 | bool == True = (moveTargets game1 {targets = new_targets1})
              | otherwise = game1 {targets = new_targets1}
        new_targets2 | bool == True = (filter (not . (isTargetKilled h_pos)) (targets game2))
                     | otherwise = (targets game2)

        -- = (filter (not . (isTargetKilled h_pos)) (targets game2))

    in game2 {targets = new_targets2}

{-
    ***  TODO ***

    Verifică dacă mai există Target-uri pe table de joc.
-}
areTargetsLeft :: Game -> Bool
areTargetsLeft game = not (null $ targets game)

{-
    *** BONUS TODO ***

    Comportamentul unui Target de a se deplasa în cerc, în jurul unui Position, având
    o rază fixată.
    Primul parametru, Position, reprezintă centrul cercului.
    Parametrul Int reprezintă raza cercului.
    Puteți testa utilizând terenul circle.txt din directorul terrains, în conjuncție
    cu funcția interactive.
-}
circle :: Position -> Int -> Behavior
circle = undefined


instance ProblemState Game Direction where
    {-
        *** TODO ***
        
        Generează succesorii stării curente a jocului.
        Utilizați advanceGameState, cu parametrul Bool ales corespunzător.
    -}
    successors state = (North, (advanceGameState North False state))
                     : (South, (advanceGameState South False state))
                     : (East, (advanceGameState East False state))
                     : (West, (advanceGameState West False state))
                     : []

    {-
        *** TODO ***
        
        Verifică dacă starea curentă este un în care Hunter-ul poate anihila
        un Target. Puteți alege Target-ul cum doriți, în prezența mai multora.
    -}
    isGoal state = (null $ (filter (isTargetKilled (hunter state)) (targets state))) == False

    {-
        *** TODO ***
        
        Euristica euclidiană (vezi hEuclidian mai jos) până la Target-ul ales
        de isGoal.
    -}
    h state
        | isGoal state == True = (hEuclidean (hunter state) (position (head (filter (isTargetKilled (hunter state)) (targets state)))))
        | otherwise = 0::Float


{-
     ** NU MODIFICATI **
-}
hEuclidean :: Position -> Position -> Float
hEuclidean (x1, y1) (x2, y2) = fromIntegral $ ((x1 - x2) ^ pow) + ((y1 - y2) ^ pow)
  where
    pow = 2 :: Int

{-
    *** BONUS ***

    Acesta reprezintă un artificiu necesar pentru testarea bonusului,
    deoarece nu pot exista două instanțe diferite ale aceleiași clase
    pentru același tip.

    OBSERVAȚIE: Testarea bonusului pentru Seach este făcută separat.
-}

newtype BonusGame = BonusGame Game
    deriving (Eq, Ord, Show)

{-
    *** BONUS TODO ***

    Folosind wrapper-ul peste tipul Game de mai sus instanțiați
    ProblemState astfel încât să fie folosită noua euristică. 
-}
instance ProblemState BonusGame Direction where
    {-
        *** BONUS TODO ***

        Pentru a ne asigura că toțî succesorii unei stări sunt de tipul
        BonusGame și folosesc noua euristică trebuie să aplicăm wrapper-ul
        definit mai sus peste toți succesorii unei stări.

        Hint: Puteți să folosiți funcția fmap pe perechi pentru acest lucru.
        https://wiki.haskell.org/Functor
    -}
    successors = undefined

    {-
        *** BONUS TODO ***

        Definiți funcția isGoal pentru BonusGame.

        Hint: Folosiți funcția isGoal deja implementată pentru tipul Game.
    -}
    isGoal = undefined

    {-
        *** BONUS TODO ***

        Definiți o funcție euristică care este capabilă să găsească un drum mai scurt
        comparativ cu cel găsit de euristica implementată pentru Game.

        ATENȚIE: Noua euristică NU trebuie să fie una trivială.

        OBSERVAȚIE: Pentru testare se va folosi fișierul terrains/game-6.txt.
    -}
    h = undefined
