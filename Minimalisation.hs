module Minimalisation where

import Data.List

import AutomatData

data MinimalisationClass = MinimalisationClass {
												number :: Int,
												classStates :: [State]
											} deriving (Eq, Ord, Show)

minimizeAutomat :: Automat -> [MinimalisationClass]
minimizeAutomat automat = initClasses automat

initClasses :: Automat -> [MinimalisationClass]
initClasses automat = [MinimalisationClass { number = 1, classStates = endStates automat }, MinimalisationClass { number = 2, classStates = (states automat \\ endStates automat) }]