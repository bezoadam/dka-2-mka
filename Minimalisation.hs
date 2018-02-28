module Minimalisation where

import Data.Foldable
import Data.List

import AutomatData

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

-- splitMinimalisationClass :: MinimalisationClass -> [MinimalisationClass]
-- splitMinimalisationClass singleClass = do
-- 							len secondClass = filter () 

-- getMinimalisationClassNumber :: [MinimalisationClass] -> State -> Int
-- getMinimalisationClassNumber minimalisationClasses checkingState = number $ filter (\x -> elem checkingState $ classStates x) minimalisationClasses !! 0

-- checkMinimalisationClass :: [MinimalisationClass] -> (MinimalisationClass, [State]) -> SplitClass
-- checkMinimalisationClass minimalisationClasses (singleClass, endStates) = do
-- 								let getMinimalisationClassNumber = getMinimalisationClassNumber minimalisationClasses
-- 								let firstStateClassNumber = getMinimalisationClassNumber $ endStates !! 0
-- 								let insideStates = filter (\x -> elem firstStateClassNumber $ classStates singleClass) endStates
-- 									SplitClass { insideStates = $ classStates singleClass \\ endStates, outsideStates =  } 

-- checkIfSameMinimalisationClass :: [MinimalisationClass] -> [State] -> Maybe [[State]]
-- checkIfSameMinimalisationClass minimalisationClasses endStates = do 
-- 													let startStates