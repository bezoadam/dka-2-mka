--DKA-2-MKA
--Bc. Adam Bezák xbezak01

module Minimalisation where	

import Data.List
import Data.Maybe

import AutomatData
import DKAParser (listnumber)

-- Datovy typ reprezentujuci jednu ekvivalencnu triedu
data MinimalisationClass = MinimalisationClass 
	{
		number :: Int,
		classStates :: [State],
		cellTransitions :: Maybe [[CellTransition]]
	} deriving (Eq, Ord, Show)

-- Datovy typ reprezentujuci jeden prechod (v tabulke)
data CellTransition = CellTransition 
	{
		startState :: Int,
		transitionValue :: String,
		endClass :: Int
	} deriving (Eq, Ord, Show)

-- Ziskanie mnoziny vsetkych stavov z mnoziny ekvivalencnych tried (na zaklade poradoveho cisla triedy)
getStatesFromMinimalisationClasses :: [MinimalisationClass] -> [State]
getStatesFromMinimalisationClasses minimalisationClasses = map (number) minimalisationClasses

-- Ziskanie startovacieho stavu z mnoziny ekvivalencnych tried
getStartStateFromMinimalisationClasses :: (State, [MinimalisationClass]) -> State
getStartStateFromMinimalisationClasses (initialStartState, minimalisationClasses) = do 
	let filteredClass = (filter (\x -> elem initialStartState $ classStates x) minimalisationClasses) !! 0
	number filteredClass

-- Ziskanie koncovych stavov z mnoziny ekvivalencnych tried
getEndStatesFromMinimalisationClasses :: ([State], [MinimalisationClass]) -> [State]
getEndStatesFromMinimalisationClasses (initialEndStates, minimalisationClasses) = do
	let filteredClasses = filter (\x -> isSubsequenceOf initialEndStates $ classStates x) minimalisationClasses
	map (number) filteredClasses

-- Ziskanie mnoziny prechodov z jednej ekvivalencnej triedy 
getTransitionsFromOneMinimalisationClass :: MinimalisationClass -> [Transition]
getTransitionsFromOneMinimalisationClass singleClass = map (\x -> Transition { from = number singleClass, to = endClass (x !! 0), value = transitionValue (x !! 0) }) (fromJust (cellTransitions singleClass))

-- Ziskanie vsetkych prechodov z ekvivalencnych tried
getTransitionsFromMinimalisationClasses :: [MinimalisationClass] -> [[Transition]]
getTransitionsFromMinimalisationClasses minimalisationClasses = map getTransitionsFromOneMinimalisationClass minimalisationClasses


-- Inicializuje pociatocne ekvivalencne triedy (vstupne a vystupne stavy)
initClasses :: Automat -> [MinimalisationClass]
initClasses automat = 	
	[	
		MinimalisationClass { number = 1, classStates = endStates automat, cellTransitions = Nothing },
	 	MinimalisationClass { number = 2, classStates = (states automat \\ endStates automat), cellTransitions = Nothing}
	]

-- Zisti ci sa dany prechod zacina s danym stavom
isStartStateInTransition :: (State, Transition) -> Bool
isStartStateInTransition (startState, transition) = do
	let fromValue = from transition
	if (startState == fromValue) then True 
	else False

-- Zisti aktualne cislo ekvivalecnej triedy
getClassNumber :: [MinimalisationClass] -> State -> Int
getClassNumber minimalisationClasses endState = number $ filter (\x -> elem endState $ classStates x) minimalisationClasses !! 0

-- Vypocita jeden prechod s danym znakom (jedna bunka v tabulke)
getCellTransition :: [MinimalisationClass] -> String -> (State, [Transition]) -> CellTransition
getCellTransition minimalisationClasses sigmaValue (startState, transitions) = do
	let transitionsWithSameStartState = filter (\x -> isStartStateInTransition(startState, x)) transitions
	let transitionWithSameStartStateAndValue = (filter (\x -> (value x) == sigmaValue) transitionsWithSameStartState) !! 0 -- jedna sa o DKA, pre kazdy stav je definovany prechod pre kazdy symbol
	CellTransition { startState = startState, transitionValue = sigmaValue, endClass = getClassNumber minimalisationClasses (to transitionWithSameStartStateAndValue)}

-- Vypocita koncove stavy v jednej ekv. triede pre jeden znak
getCellTransitions :: [MinimalisationClass] -> String -> ([State], [Transition]) -> [CellTransition]
getCellTransitions minimalisationClasses sigmaValue (startStates, transitions) = do
	map (\x -> getCellTransition minimalisationClasses sigmaValue (x, transitions)) startStates

-- Updatuje jednu ekvivalencnu triedu
updateMinimalisationClass :: [MinimalisationClass] -> ([String], [Transition]) -> MinimalisationClass -> MinimalisationClass
updateMinimalisationClass minimalisationClasses (sigma,delta) singleClass = do
	let cellTransitions = map (\x -> getCellTransitions minimalisationClasses x (classStates singleClass, delta)) sigma
	MinimalisationClass { number = number singleClass, classStates = classStates singleClass, cellTransitions = Just cellTransitions }

-- Zaktualizuje "bunky v tabuke" ekvivalencnych tried
updateMinimalisationClasses :: Automat -> [MinimalisationClass] -> [MinimalisationClass]
updateMinimalisationClasses automat minimalisationClasses = do 
	map (updateMinimalisationClass minimalisationClasses (sigma automat, delta automat)) minimalisationClasses

-- Vrati bool ak sa cislo triedy nerovna aktualnemu cislu ekvivalecnej triedy
getNewTransitionClass :: Int -> CellTransition -> Bool
getNewTransitionClass classNumber cellTransition = do
	if (classNumber /= endClass cellTransition) then True
	else False

-- Vrati bud nove vytvorene prechody alebo vrati povodne 
splitTransitionClasses :: [CellTransition] -> [CellTransition]
splitTransitionClasses cellTransitions = do
	let firstEndClass = endClass $ cellTransitions !! 0
	let getNewTransitionClassWithNumber = getNewTransitionClass firstEndClass
	let otherTransitionClasses = filter (getNewTransitionClassWithNumber) cellTransitions
	if (listnumber otherTransitionClasses /= 0) then otherTransitionClasses
	else cellTransitions

-- Vrati startovacie stavy pre prechody v jednej ekv. triede
getStartStates :: [CellTransition] -> [State]
getStartStates cellTransitions = map (\x -> startState x) cellTransitions

-- Vrati rozdiel dvoch prechodov 
findDifference :: [[CellTransition]] -> [[CellTransition]] -> Maybe [CellTransition]
findDifference [] _ = Nothing
findDifference _ [] = Nothing
findDifference (x:xs) (y:ys) = 	if x /= y then Just y else findDifference xs ys

-- Vrati novu ekvivalencnu triedu ak je co stiepit, v pripade ze uz mame MKA vrati Nothing
splitMinimalisationClass :: (Int,[MinimalisationClass]) -> Maybe (Int, MinimalisationClass)
splitMinimalisationClass (_,[]) = Nothing
splitMinimalisationClass (numberOfClasses, (x:xs)) = do 
	let cellTransitionsNew = fromJust (cellTransitions x)
	let splittedTransitionClasses = map (\y -> splitTransitionClasses y) cellTransitionsNew
	let difference = findDifference cellTransitionsNew splittedTransitionClasses
	case difference of
		Just difference ->  Just(number x, MinimalisationClass { number = numberOfClasses + 1, classStates = getStartStates difference, cellTransitions = Nothing })
		Nothing -> splitMinimalisationClass (numberOfClasses, xs)

-- Pociatocna funkcia ktora rekurzivne rozdeluje ekvivalencne triedy
splitClasses :: Automat -> [MinimalisationClass] -> [MinimalisationClass]
splitClasses automat minimalisationClasses = do
	case splitMinimalisationClass (listnumber minimalisationClasses, minimalisationClasses) of
		Just (originalClassNumber, splittedClass) -> do
			let filteredClass = filter (\x -> number x == originalClassNumber) minimalisationClasses !! 0
			let newClass = MinimalisationClass { number = number filteredClass, classStates = classStates filteredClass \\ classStates splittedClass, cellTransitions = cellTransitions filteredClass }
			let newClasses = replace' filteredClass newClass minimalisationClasses
			let newNewClasses = newClasses ++ [splittedClass]
			let updatedNewClasses = updateMinimalisationClasses automat newNewClasses
			splitClasses automat updatedNewClasses
		Nothing -> minimalisationClasses

-- Pomocna funkcia ktora nahradi prvok v liste inym prvkom
replace' :: Eq b => b -> b -> [b] -> [b]
replace' a b = map (\x -> if (a == x) then b else x)