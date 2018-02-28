module Minimalisation where

import Data.Foldable
import Data.List

import AutomatData

data MinimalisationClass = MinimalisationClass {
												number :: Int,
												classStates :: [State],
												cellTransitions :: Maybe [CellTransition]
											} deriving (Eq, Ord, Show)

data CellTransition = CellTransition {
										transitionValue :: String,
										endClass :: [Int]
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

isStartStateInTransition :: ([State], Transition) -> Bool
isStartStateInTransition (startStates, transition) = do
											let fromValue = from transition
											if (elem fromValue startStates) then True 
											else False

getClassNumber :: [MinimalisationClass] -> State -> Int
getClassNumber minimalisationClasses endState = number $ filter (\x -> elem endState $ classStates x) minimalisationClasses !! 0



-- Vypocita koncove stavy v jednej ekv. triede pre jeden znak
getCellTransition :: [MinimalisationClass] -> String -> ([State], [Transition]) -> CellTransition
getCellTransition minimalisationClasses sigmaValue (startStates, transitions) = do
												let transitionsWithSameStartState = filter (\x -> isStartStateInTransition(startStates, x)) transitions
												let transitionsWithSameStartStateAndValue = filter (\x -> (value x) == sigmaValue) transitionsWithSameStartState
												let endStates = map (\x -> to x) transitionsWithSameStartStateAndValue
												CellTransition { transitionValue = sigmaValue, endClass = map (\x -> getClassNumber minimalisationClasses x) endStates }

-- Updatuje jednu ekvivalencnu triedu
updateMinimalisationClass :: [MinimalisationClass] -> ([String], [Transition]) -> MinimalisationClass -> MinimalisationClass
updateMinimalisationClass minimalisationClasses (sigma,delta) singleClass = do
												let cellTransitions = map (\x -> getCellTransition minimalisationClasses x (classStates singleClass, delta)) sigma
												MinimalisationClass { number = number singleClass, classStates = classStates singleClass, cellTransitions = Just cellTransitions }

splitClasses :: Automat -> [MinimalisationClass] -> [MinimalisationClass]
splitClasses automat minimalisationClasses = map (updateMinimalisationClass minimalisationClasses (sigma automat, delta automat)) minimalisationClasses
				
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