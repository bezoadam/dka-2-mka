module Minimalisation where	

import Data.Foldable
import Data.List
import Data.Maybe
import Debug.Trace

import AutomatData
import DKAParser (listnumber)

data MinimalisationClass = MinimalisationClass {
												number :: Int,
												classStates :: [State],
												cellTransitions :: Maybe [[CellTransition]]
											} deriving (Eq, Ord, Show)

data CellTransition = CellTransition {
										startState :: Int,
										transitionValue :: String,
										endClass :: Int
									} deriving (Eq, Ord, Show)

data SplitClass = SplitClass {
								insideStates :: [State],
								outsideStates :: Maybe [State]
							} deriving (Eq, Ord, Show)

debug = flip trace

--Pre kazdu triedu zoberiem kazdy stav a pre kazdy znak z abecedy
-- vyskusakm do akej triedy padne. Ak nejaky padne do inej triedy nez ta prva tak ten stav
-- odstepim a dam do novej minimalizacnej triedy. + musim kontrolovat ci sa uz nenachadza
-- v minimalizacnej triede z predchadzajuceho stepenia ( z predchadzajuceho pismena)
-- Ukoncim ak v kazdej min. triede sa vsetky  znaky dostanu do tej istej triedy
minimizeAutomat :: Automat -> [MinimalisationClass]
minimizeAutomat automat = initClasses automat

appendToList :: a -> [a] -> [a]
appendToList a [] = [a]
appendToList a (x:xs) = x : appendToList a xs

initClasses :: Automat -> [MinimalisationClass]
initClasses automat = 	[	
							MinimalisationClass { number = 1, classStates = endStates automat, cellTransitions = Nothing },
						 	MinimalisationClass { number = 2, classStates = (states automat \\ endStates automat), cellTransitions = Nothing}
						]

isStartStateInTransition :: (State, Transition) -> Bool
isStartStateInTransition (startState, transition) = do
											let fromValue = from transition
											if (startState == fromValue) then True 
											else False

getClassNumber :: [MinimalisationClass] -> State -> Int
getClassNumber minimalisationClasses endState = number $ filter (\x -> elem endState $ classStates x) minimalisationClasses !! 0


getCellTransition :: [MinimalisationClass] -> String -> (State, [Transition]) -> CellTransition
getCellTransition minimalisationClasses sigmaValue (startState, transitions) = do
												let transitionsWithSameStartState = filter (\x -> isStartStateInTransition(startState, x)) transitions
												let transitionWithSameStartStateAndValue = (filter (\x -> (value x) == sigmaValue) transitionsWithSameStartState) !! 0
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

updateMinimalisationClasses :: Automat -> [MinimalisationClass] -> [MinimalisationClass]
updateMinimalisationClasses automat minimalisationClasses = do 
										map (updateMinimalisationClass minimalisationClasses (sigma automat, delta automat)) minimalisationClasses

getNewTransitionClass :: Int -> CellTransition -> Bool
getNewTransitionClass classNumber cellTransition = do
							if (classNumber /= endClass cellTransition) then True
							else False


splitTransitionClasses :: (Int, [CellTransition]) -> [CellTransition]
splitTransitionClasses (classNumber, cellTransitions) = do
										let firstEndClass = endClass $ cellTransitions !! 0
										let getNewTransitionClassWithNumber = getNewTransitionClass classNumber
										let otherTransitionClasses = filter (getNewTransitionClassWithNumber) cellTransitions
										if (listnumber otherTransitionClasses /= 0) then otherTransitionClasses
										else cellTransitions

getStartStates :: [CellTransition] -> [State]
getStartStates cellTransitions = map (\x -> startState x) cellTransitions

findDifference :: [[CellTransition]] -> [[CellTransition]] -> Maybe [CellTransition]
findDifference [] _ = Nothing
findDifference _ [] = Nothing
findDifference (x:xs) (y:ys) = 	if x /= y then Just y else findDifference xs ys

splitMinimalisationClass :: [MinimalisationClass] -> Maybe (Int, MinimalisationClass)
splitMinimalisationClass [] = Nothing
splitMinimalisationClass (x:xs) = do 
							let cellTransitionsNew = fromJust (cellTransitions x)
							let splittedTransitionClasses = map (\y -> splitTransitionClasses (number x, y)) cellTransitionsNew
							let difference = findDifference cellTransitionsNew splittedTransitionClasses
							case difference of
								Just difference -> Just(number x, MinimalisationClass { number = (number x) + 1, classStates = getStartStates difference, cellTransitions = Nothing })
								Nothing -> splitMinimalisationClass xs

splitClasses :: Automat -> [MinimalisationClass] -> [MinimalisationClass]
splitClasses automat minimalisationClasses = do
					case splitMinimalisationClass minimalisationClasses of
						Just (originalClassNumber, splittedClass) -> do
							let filteredClass = filter (\x -> number x == originalClassNumber) minimalisationClasses !! 0
							let newClass = MinimalisationClass { number = number splittedClass, classStates = classStates filteredClass \\ classStates splittedClass, cellTransitions = cellTransitions filteredClass }
							let newClasses = replace' filteredClass newClass minimalisationClasses
							let newNewClasses = newClasses ++ [splittedClass]
							let updatedNewClasses = updateMinimalisationClasses automat newNewClasses
							--TODO FIX NUMBER
							splitClasses automat updatedNewClasses
							trace ("MINIMALCLASSES: \n" ++ show updatedNewClasses ++ "    \n REPLACE FILTERED CLASS:    \n " ++ show filteredClass ++ "    \n   REPLACE NEW CLASS          \n"  ++ show newClass) (splitClasses automat updatedNewClasses)
						Nothing -> minimalisationClasses


replace' :: Eq b => b -> b -> [b] -> [b]
replace' a b = map (\x -> if (a == x) then b else x)

