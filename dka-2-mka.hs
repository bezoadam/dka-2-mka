--DKA-2-MKA
--Adam Bezák xbezak01

import Control.Monad.State
import qualified Data.IntMap as M
import Data.Word
import System.Environment
import System.Directory
import System.Exit
import System.Console.GetOpt
import Data.Maybe
import Data.Typeable
import Data.List
import Data.List.Split
import Data.Char

import FileParser

data Options = Options
	{ 
		optShowDKA  :: Maybe FilePath, 
		optShowMKA	:: Maybe FilePath
	} deriving Show

defaultOptions = Options
	{
		optShowDKA  = Nothing,
		optShowMKA	= Nothing
	}

options :: [OptDescr (Options -> Options)]
options =
	[ Option ['i']	[]
	    (OptArg ((\ f opts -> opts { optShowDKA = Just f }) . fromMaybe "ShowDKA") "FILENAME")
	    "Load and write DKA on file output or STDOUT."
	, Option ['t'] []
	    (OptArg ((\ f opts -> opts { optShowMKA = Just f }) . fromMaybe "ShowMKA") "FILENAME")
	    "Load and transform DKA into MKA and write on file output or STDOUT."
	]

compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
 	case getOpt Permute options argv of
     	(o,n,[]) -> return (foldl (flip id) defaultOptions o, n)
     	(_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
 	where header = "Usage: OPTION [FILENAME]"

-- Dlzka listu
listnumber :: [String] -> Int 
listnumber [] = 0
listnumber (x:xs) = 1 + listnumber xs

-- Nacitanie DKA do vnutornej reprezentacie
loadDKA :: [String] -> (String, String, String, [String])
loadDKA customWords = (customWords !! 0, customWords !! 1, customWords !! 2, drop 3 customWords)

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
							let ruleList = removeItemFromList "," $ splitOn "," rule
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
							let checkRulePredicate = checkRuleFormat allStatesList
							all (checkRulePredicate) rules 

main = do
	argv <- getArgs
	(opts, filenames) <- compilerOpts argv

	when (listnumber filenames > 1) $ do
		error "Too much files."

	when ((not $ isNothing $ optShowDKA opts) && (not $ isNothing $ optShowMKA opts)) $ do
		if null filenames
			then do
				print "SHOWDKA, SHOWMKA STDOUT"
				exitSuccess
			else do
				print "SHOWDKA, SHOWMKA FILE"
				exitSuccess

	when (not $ isNothing $ optShowDKA opts) $ do
		if null filenames
			then do
				print "SHOWDKA STDOUT"
				exitSuccess
			else do
				let filename = head filenames
				lines <- customFileParser filename
				let (allStates, startState, endStates, rules) = loadDKA $ words lines
				let allStatesList = splitOn "," allStates
				let startStateList = splitOn "," startState
				let endStatesList = splitOn "," endStates

				if (checkStatesFormat allStatesList && checkStartState startStateList && 
					checkStatesFormat endStatesList && checkIfSublist (startStateList, allStatesList) && 
					checkIfSublist (endStatesList, allStatesList) && checkRules (rules, allStatesList)) then do
						print "Spravny DKA"
				else do
					print "Chybny DKA"
				exitSuccess

	when (not $ isNothing $ optShowMKA opts) $ do
		if null filenames
			then do
				print "SHOWMKA STDOUT"
				exitSuccess
			else do
				print "SHOWDKA FILE"
				exitSuccess

	error "You need to specify one argument."
	exitFailure