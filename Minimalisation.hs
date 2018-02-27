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
												endStatesHelper :: [State]
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

getCellTransition :: String -> ([State], [Transition]) -> CellTransition
getCellTransition sigmaValue (startStates, transitions) = do
												let transitionsWithSameStartState = filter (\x -> isStartStateInTransition(startStates, x)) transitions
												let transitionsWithSameStartStateAndValue = filter (\x -> (value x) == sigmaValue) transitionsWithSameStartState
												CellTransition { transitionValue = sigmaValue, endStatesHelper = map (\x -> to x) transitionsWithSameStartStateAndValue }

updateMinimalisationClass :: ([String], [Transition]) -> MinimalisationClass -> MinimalisationClass
updateMinimalisationClass (sigma,delta) singleClass = singleClass
												

splitClasses :: Automat -> [MinimalisationClass] -> [MinimalisationClass]
splitClasses automat minimalisationClasses = map (updateMinimalisationClass (sigma automat, delta automat)) minimalisationClasses