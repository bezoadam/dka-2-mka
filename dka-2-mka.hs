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

-- Kontrola spravneho formatu vsetkyh stavov
checkAllStatesFormat :: String -> Bool
checkAllStatesFormat states = all isInteger $ splitOn "," states

-- Kontrola spravneho formatu pociatocneho stavu
checkStartState :: String -> Bool
checkStartState startState = if ((length $ splitOn "," startState) == 1)
								then do 
									let first = (splitOn "," startState) !! 0
									if (isInteger $ first) then True
										else False 
								else False


main = do
	argv <- getArgs
	(opts, filenames) <- compilerOpts argv
	print opts 
	print filenames

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
				print "SHOWDKA FILE"
				let filename = head filenames
				lines <- customFileParser filename
				let (allStates, startState, endStates, rules) = loadDKA $ words lines
				print (allStates, startState, endStates, rules)
				print $ checkAllStatesFormat allStates
				print $ checkStartState startState
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