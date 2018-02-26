module DKAParser where

import Data.List
import Data.Char

import AutomatData

------------------------------DKA PARSER----------------------------------

-- Rozdeli String na zaklade delimetra
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

-- Dlzka listu
listnumber :: [String] -> Int 
listnumber [] = 0
listnumber (x:xs) = 1 + listnumber xs

-- Nacitanie DKA do vnutornej reprezentacie
loadDKA :: [String] -> ([String], [String], [String], [String])
loadDKA customWords = (wordsWhen (==',') $ customWords !! 0, wordsWhen (==',') $ customWords !! 1, wordsWhen (==',') $ customWords !! 2, drop 3 customWords)

-- Zistenie ci String je Integer
isInteger s = case reads s :: [(Integer, String)] of
  	[(_, "")] -> True
  	_         -> False

-- Kontrola spravneho formatu stavov
checkStatesFormat :: [String] -> Bool
checkStatesFormat states = all isInteger $ states

-- Kontrola spravneho formatu pociatocneho stavu
checkStartState :: [String] -> Bool
checkStartState startState = if ((length $ startState) == 1)
								then do 
									let first = startState !! 0
									if (isInteger $ first) then True
										else False 
								else False

-- Kontrola ci dane stavy sa nachadzaju vo vsetkych stavoch
checkIfSublist :: ([String], [String]) -> Bool
checkIfSublist (substates, states) =  isSubsequenceOf substates states

-- Odstrani element z listu
removeItemFromList :: Eq a => a -> [a] -> [a]
removeItemFromList a list = [x | x <- list, x /= a]

-- Predikat na overenie ci je pravidlo v spravnom tvare
checkRuleFormat :: [String] -> String -> Bool
checkRuleFormat allStates rule = do
							let ruleList = removeItemFromList "," $ wordsWhen (==',') rule
							if (((length $ ruleList) == 3) && ((length $ ruleList !! 1) == 1))
								then do
									let startState = ruleList !! 0
									let symbol = head $ ruleList !! 1
									let endState = ruleList !! 2
									if (isInteger startState && isInteger endState && isAsciiLower symbol)
										then do
											if (elem startState allStates &&  elem endState allStates) then True
												else False
										else False
							else False


-- Skontroluje spravnost vsetkych stavov
checkRules :: ([String], [String]) -> Bool
checkRules (rules, allStatesList) = do
							if (length rules /= 0) then do 
								let checkRulePredicate = checkRuleFormat allStatesList
								all (checkRulePredicate) rules
							else False

isDKAValid :: ([String], [String], [String], [String]) -> Bool
isDKAValid (allStatesList, startStateList, endStatesList, rules) = do
																if (checkStatesFormat allStatesList && checkStartState startStateList && 
																	checkStatesFormat endStatesList && checkIfSublist (startStateList, allStatesList) && 
																	checkIfSublist (endStatesList, allStatesList) && checkRules (rules, allStatesList)) then True
																else False