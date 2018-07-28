{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances #-}

module GAS where

import ProblemState

import qualified Data.Map.Strict as M
import Data.List
import Data.Char

{-
    Pozițiile tablei de joc, în formă (linie, coloană), unde ambele coordonate
    pot fi negative.
-}
type Position = (Int, Int)


{-
    Culorile pătratelor și cercurilor.
-}
data Color = Red | Blue | Gray  
    deriving (Eq, Ord)

instance Show Color where
	show Red = "R"
	show Blue = "B"
	show Gray = "G"

{-
    Orientările pătratelor și săgeților.
-}
data Heading = North | South | East | West
    deriving (Eq, Ord)

instance Show Heading where
    show North = "^"
    show South = "v"
    show East  = ">"
    show West  = "<"

{-
    *** TODO ***

    Un obiect de pe tabla de joc: pătrat/ cerc/ săgeată.
-}
data Object = Patrat Color Heading | Cerc Color | Sageata Heading
    deriving (Eq, Ord)

{-
    *** TODO ***

    Reprezetarea textuală a unui obiect.
-}

stringToLower :: [Char] -> [Char]
stringToLower s = map toLower s

instance Show Object where
	show (Patrat color heading) = (show color) ++ (show heading)
	show (Cerc color) = stringToLower (show color)
	show (Sageata heading) = show heading
	 

{-
    *** TODO ***

    Un nivel al jocului.

    Recomandăm Data.Map.Strict.-
-}



data Level = Level (M.Map Position [Object])
    deriving (Eq, Ord)


{-
    *** TODO ***

    Reprezetarea textuală a unui nivel.
-}

removeChar :: String -> String
removeChar xs = [x | x <- xs, not (x `elem` "[],Just")]

removeSpace :: String -> String
removeSpace xs = [x | x <- xs, not (x `elem` " ")]

generateIndexes :: Int -> Int -> Int -> Int -> [Position]
generateIndexes startX endX startY endY = [(x,y) | x <- [startX..endX], y <- [startY .. endY]]

showObjectList :: [Object] -> [Char]
showObjectList list = if (length list == 2)
							then show list
							else if (verifyType (head list)) == 'p'
									then (show (head list)) ++ " "
									else "  " ++ (show (head list))

notAloneOnLine ::  M.Map Position [Object] -> Position -> Bool
notAloneOnLine lvl pos = (fst pos) `elem` (map fst (delete pos (M.keys lvl)))

afisare :: [Position] -> [Position] -> M.Map Position [Object] ->Int -> Int-> [[Char]]

afisare allPos ocPos l endY endX = map myFunc allPos where
			    	myFunc pos = if  pos `elem` ocPos 
						then if ( ((snd pos) == endY) && ((fst pos) /= endX)) 
								then (showObjectList (M.findWithDefault [] pos l ) ++ "\n" )
								else if (((snd pos) == endY) && ((fst pos) == endX))
										then (showObjectList (M.findWithDefault [] pos l ))
										else (showObjectList (M.findWithDefault [] pos l ) ++ "|")
							 
						else if ((snd pos) == endY && ((fst pos) /= endX))
							then ("   \n")
							else if (((snd pos) == endY) && ((fst pos) == endX))
									then ("   ")
									else ("   |")

afisare2 :: [Position] -> [Position] ->  M.Map Position [Object] -> Int-> Int -> [Char]
afisare2 allPos ocPos l endY endX = removeChar (intercalate "" (afisare allPos ocPos l endY endX))

takeStartX :: [Position] -> Int
takeStartX posList = fst (head posList)

takeEndX :: [Position] -> Int
takeEndX posList = fst (posList !! ((length posList) - 1) )

takeStartY :: [Position] -> Int
takeStartY posList = minimum ( map snd posList  )

takeEndY :: [Position] -> Int
takeEndY posList = maximum ( map snd posList  )			

instance Show Level where
    show (Level lvl) = afisare2 (generateIndexes (takeStartX k) (takeEndX k) (takeStartY k) ((takeEndY k))) k lvl (takeEndY k) (takeEndX k) where
		k = M.keys lvl

{-
    *** TODO ***

    Nivelul vid, fără obiecte.
-}
emptyLevel :: Level
emptyLevel = Level (M.empty)

{-
	Functii auxiliare utilizate la adaugare
-}

verifyType :: Object -> Char
verifyType (Patrat _ _) = 'p'
verifyType (Cerc _ ) = 'c'
verifyType (Sageata _) = 's'

--let function el = if  (verifyType (fst el)) == 'p' then obj : el else el : obj


prelucrareElem ::Object -> [Object] -> [Object]
prelucrareElem obj listObj = if (verifyType obj) == 'p'
								then [obj] ++ listObj
								else listObj ++ [obj]

adaugare:: Object -> Position ->M.Map Position [Object] -> M.Map Position [Object]
adaugare obj pos lvl = if (M.member pos lvl ) /= False
							then M.insert pos (prelucrareElem obj (M.findWithDefault [] pos lvl )) lvl
							else M.insert pos [obj] lvl  

{-
    *** TODO ***

    Adaugă un pătrat cu caracteristicile date la poziția precizată din nivel.
-}
addSquare :: Color -> Heading -> Position -> Level -> Level
addSquare color heading pos  (Level lvl) = Level (adaugare (Patrat color heading) pos lvl)


{-
    *** TODO ***

    Adaugă un cerc cu caracteristicile date la poziția precizată din nivel.
-}
addCircle :: Color -> Position -> Level -> Level
addCircle color pos (Level lvl) = Level (adaugare (Cerc color) pos lvl)

{-
    *** TODO ***

    Adaugă o săgeată cu caracteristicile date la poziția precizată din nivel.
-}
addArrow :: Heading -> Position -> Level -> Level
addArrow heading pos (Level lvl)= Level (adaugare (Sageata heading) pos lvl)
 

{-
    *** TODO ***

    Mută pătratul de la poziția precizată din nivel. Dacă la poziția respectivă
    nu se găsește un pătrat, întoarce direct parametrul.
-}

calculateNewPos :: Object -> Position -> Position
calculateNewPos (Patrat color heading) oldPos = if heading == North 
													then (fst(oldPos) - 1, snd(oldPos) )
													else if heading == South
															then (fst(oldPos) + 1, snd(oldPos) )
															else if heading == East
																	then (fst(oldPos) , snd(oldPos) + 1)
																	else (fst(oldPos) , snd(oldPos) - 1)




--editez ce era la pozitia la care mut
editTarget :: [Object] -> Object -> [Object]
editTarget listObj obj =  [obj] ++ listObj

changeOrient :: Object -> Object -> Object
changeOrient (Patrat c h1) (Sageata h2) = (Patrat c h2)

{-- ajustez lista de obiecte in functie de unde se va aseza obiectul pe care il mut:
	daca se aseaza pe spatiu gol, intorc o lista formata doar din acel obiect, daca se
aseaza pe cerc intorc o lista cu acel obiect si cercul, iar daca se aseaza pe sageata
intorc o lista cu patratul cu directia schimbata si sageata 
--> folosita la mutarea recursiva a patratelor
--}
prepareToInsert :: Object -> [Object] -> [Object]
prepareToInsert patrat lista = if (length lista) == 0
								then [patrat]
								else if (verifyType (last lista)) == 'c'
										then [patrat] ++ [(last lista)]
										else if (verifyType (last lista) ) == 's'
												then [(changeOrient patrat (last lista))] ++ [(last lista)]
												else [patrat] 

{-- Verifica toate cazurile de pozitii pe care poate pica patratul si face o inserare corespunzatoare
Daca se insereaza pe un loc gol, pe un cerc sau pe o sageata, se fac prelucrarile corespunzatoare
Daca locul este deja ocupat de un ptrat, atunci se apeleaza recursiv functia de mutare--}

allCases ::  Position -> Position -> Object ->Level ->Char->M.Map Position [Object]
allCases curPos targetPos obj (Level lvl) direction = if (length (M.findWithDefault [] targetPos lvl )) == 0
										then M.insert targetPos [obj] lvl
										else if  (verifyType (last (M.findWithDefault [] targetPos lvl ))) == 'c'
												then M.insert targetPos (editTarget (M.findWithDefault [] targetPos lvl ) obj) lvl
												else if (verifyType (last (M.findWithDefault [] targetPos lvl ))) == 's'
														then M.insert targetPos (editTarget (M.findWithDefault [] targetPos lvl ) (changeOrient obj (last (M.findWithDefault [] targetPos lvl )))) lvl
														else if (verifyType (head (M.findWithDefault [] targetPos lvl ))) == 'p'
																then M.insert targetPos (prepareToInsert obj (M.findWithDefault [] targetPos lvl )) (move2 targetPos direction (Level lvl))
																else M.insert targetPos [obj] lvl		


{-- Sterge patratul care se muta de la pozitia initiala --}
deleteFromLvl :: Level -> Object -> Position -> Level
deleteFromLvl (Level lvl) obj pos = if (length (M.findWithDefault [] pos lvl)) == 1
										then Level(M.delete pos lvl)
										else Level (M.insert pos (delete obj (M.findWithDefault [] pos lvl)) lvl )

{-- Calucleaza noul heading al unui patrat un functie de sageata pe care acesta se afla--}
calculateNewPos2 :: Char -> Position ->Position
calculateNewPos2 heading oldPos = if heading == 'N'
									then (fst(oldPos) - 1, snd(oldPos) )
									else if heading == 'S'
											then (fst(oldPos) + 1, snd(oldPos) )
											else if heading == 'E'
													then (fst(oldPos) , snd(oldPos) + 1)
													else (fst(oldPos) , snd(oldPos) - 1)

detDirType :: Object -> Char
detDirType (Patrat c h) = if h == South 
							then 'S'
							else if h == North
									then 'N'
									else if h == East
											then 'E'
											else 'W'


{--Functie auxiliare de move pentru gestionarea mai usoara a tipurilor--}
move2 :: Position  -> Char -> Level  -> M.Map Position [Object]  

move2 pos direction (Level lvl) = if (length  (M.findWithDefault [] pos lvl)) == 0
									then lvl
									else if (verifyType  (head (M.findWithDefault [] pos lvl ))) /= 'p'
											then  lvl
											else allCases pos (calculateNewPos2 direction pos)(head (M.findWithDefault [] pos lvl)) (deleteFromLvl (Level lvl) (head (M.findWithDefault [] pos lvl)) pos) direction


move :: Position  -- Poziția
     -> Level     -- Nivelul inițial
     -> Level     -- Nivelul final


move pos (Level lvl) = Level (move2 pos  (detDirType (head (M.findWithDefault [] pos lvl)))  (Level lvl))	


{-
    *** TODO ***

    Instanțiați clasa `ProblemState` pentru jocul nostru.
-}

{--Functie care verifica daca fiecare patrat existent se afla pe un cerc--}
verifyAllPos :: Level -> [Bool]
verifyAllPos (Level lvl) = map (\e -> if ((length e) == 1) && ((verifyType (head e)) == 'p')
					then False
					else if ((length e) == 2) && (((verifyType(head e)) == 'p') && ((verifyType(last e)) == 'c'))
						then True
						else if (length e) == 2
							then False
							else True) (M.elems lvl)

posWithSq :: Level -> [Position]
posWithSq (Level lvl ) = M.keys (M.filter (\obj -> verifyType((head obj)) == 'p') lvl)


instance ProblemState Level Position where
	{--Aplic move pe fiecare pozitie a niveluluipe care se afla un patra => succesorii nivelului-}
    successors (Level lvl) = zipWith (,) (posWithSq (Level lvl)) (map (\p -> move p (Level lvl)) (posWithSq (Level lvl)) )


	{-- Se verifica daca lista de bool intoarsa de verifyAllPos are doar True adica daca fiecare patrat este pe un cerc--}
    isGoal lvl  = all (\e -> e == True) (verifyAllPos lvl)

    -- Doar petru BONUS
    -- heuristic =
