--DKA-2-MKA
--Bc. Adam Bezák xbezak01

import Control.Monad.State
import System.Environment
import System.Exit
import System.Console.GetOpt
import System.IO
import Data.Maybe
import Data.Typeable
import Data.List
import Debug.Trace

import FileParser
import Minimalisation
import DKAParser
import AutomatData

-------------------------------ARGUMENTS----------------------

-- Datovy typ reprezentujuci cestu k zadanym vstupnym suborom
data Options = Options
	{ 
		optShowDKA  :: Maybe FilePath, 
		optShowMKA	:: Maybe FilePath
	} deriving Show

-- Defaultne nezadane ziadne vstupne subory
defaultOptions = Options
	{
		optShowDKA  = Nothing,
		optShowMKA	= Nothing
	}


options :: [OptDescr (Options -> Options)]
options =
	[ Option ['i']	[]
	    (OptArg ((\ f opts -> opts { optShowDKA = Just f }) . fromMaybe "ShowDKA") "FILENAME")
	    "Nacitanie a vypis vstupneho DKA na standartni vystup."
	, Option ['t'] []
	    (OptArg ((\ f opts -> opts { optShowMKA = Just f }) . fromMaybe "ShowMKA") "FILENAME")
	    "Nacitanie a transformacia DKA na MKA a vypis MKA na standartni vystup."
	]

-- Parsovanie vstupnych argumentov
compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
 	case getOpt Permute options argv of
     	(o,n,[]) -> return (foldl (flip id) defaultOptions o, n)
     	(_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
 	where header = "Pouzitie: OPTION [FILENAME]"

-------------------------------OUTPUT-------------------------------

-- Ziskanie mnoziny stavov v pozadovanom tvare
printStates :: Show a => [a] -> String
printStates = intercalate "," . map show

-- Ziskanie prechodu v pozadovanom tvare
printTransitions :: Transition -> String
printTransitions transition = show (from transition) ++ "," ++ value transition ++ "," ++ show (to transition)

-- Ziskanie minimalneho KA z povodneho DKA a ekv. tried
getMKA :: (Automat, [MinimalisationClass]) -> Automat
getMKA (automat, minimalisationClasses) = do 
	let minimalStates = getStatesFromMinimalisationClasses minimalisationClasses
	let minimalStartState = getStartStateFromMinimalisationClasses (initialState automat, minimalisationClasses)
	let minimalEndStates = getEndStatesFromMinimalisationClasses (endStates automat, minimalisationClasses)
	let minimalTransitions = concat $ getTransitionsFromMinimalisationClasses minimalisationClasses
	let transitionsStrings = map printTransitions minimalTransitions
	Automat { states = minimalStates, sigma = sigma automat, delta = minimalTransitions, initialState = minimalStartState, endStates = minimalEndStates }

-------------------------------INPUT--------------------------------

--Nacita vstupne pravidla zo stdin
getRules :: IO [String]
getRules = go ""
	where go contents = do
		done <- isEOF
		if done 
			then return $ lines contents
		else do
			line <- getLine
			go (contents ++ line ++ "\n")

-------------------------------MAIN---------------------------------

main = do
	argv <- getArgs
	(opts, filenames) <- compilerOpts argv

	when (listnumber filenames > 1) $ do
		error "Privela vstupnych suborov."

	-- "-i -t"
	when ((not $ isNothing $ optShowDKA opts) && (not $ isNothing $ optShowMKA opts)) $ do
		if null filenames
			then do
				allStates <- getLine
				startState <- getLine
				endStatesInput <- getLine
				rules <- getRules
				
				let allStatesList = wordsWhen (==',') allStates
				let startStateList = wordsWhen (==',') startState
				let endStatesList = wordsWhen (==',') endStatesInput

				case loadAutomatData (allStatesList, startStateList, endStatesList, rules) of
					Just automat -> do
						putStrLn $ id (printStates $ states automat)
						print $ initialState automat
						putStrLn $ id (printStates $ endStates automat)
						let transitionsStrings = map printTransitions $ delta automat
						mapM_ (\x -> putStrLn $ id x) transitionsStrings

						putStrLn ""

						let updatedAutomat = (updateAutomat automat)
						let classes = updateMinimalisationClasses updatedAutomat $ initClasses updatedAutomat
						let minimalisationClasses = splitClasses updatedAutomat classes
						let minimalAutomat = getMKA (updatedAutomat, minimalisationClasses)
						putStrLn $ id (printStates $ states minimalAutomat)
						print $ initialState minimalAutomat
						putStrLn $ id (printStates $ endStates minimalAutomat)
						let transitionsStrings = map printTransitions $ delta minimalAutomat
						mapM_ (\x -> putStrLn $ id x) transitionsStrings
						exitSuccess
					Nothing -> error "Chybne zadany DKA."
			else do
				let filename = head filenames
				lines <- customFileParser filename
				let (allStatesList, startStateList, endStatesList, rules) = loadDKA $ words lines

				case loadAutomatData (allStatesList, startStateList, endStatesList, rules) of
					Just automat -> do
						putStrLn $ id (printStates $ states automat)
						print $ initialState automat
						putStrLn $ id (printStates $ endStates automat)
						let transitionsStrings = map printTransitions $ delta automat
						mapM_ (\x -> putStrLn $ id x) transitionsStrings

						putStrLn ""

						let updatedAutomat = (updateAutomat automat)
						let classes = updateMinimalisationClasses updatedAutomat $ initClasses updatedAutomat
						let minimalisationClasses = splitClasses updatedAutomat classes
						let minimalAutomat = getMKA (updatedAutomat, minimalisationClasses)
						putStrLn $ id (printStates $ states minimalAutomat)
						print $ initialState minimalAutomat
						putStrLn $ id (printStates $ endStates minimalAutomat)
						let transitionsStrings = map printTransitions $ delta minimalAutomat
						mapM_ (\x -> putStrLn $ id x) transitionsStrings
						exitSuccess
					Nothing -> error "Chybne zadany DKA."
	-- "-i"
	when (not $ isNothing $ optShowDKA opts) $ do
		if null filenames
			then do
				allStates <- getLine
				startState <- getLine
				endStatesInput <- getLine
				rules <- getRules
				
				let allStatesList = wordsWhen (==',') allStates
				let startStateList = wordsWhen (==',') startState
				let endStatesList = wordsWhen (==',') endStatesInput

				case loadAutomatData (allStatesList, startStateList, endStatesList, rules) of
					Just automat -> do
						putStrLn $ id (printStates $ states automat)
						print $ initialState automat
						putStrLn $ id (printStates $ endStates automat)
						let transitionsStrings = map printTransitions $ delta automat
						mapM_ (\x -> putStrLn $ id x) transitionsStrings
						exitSuccess
					Nothing -> error "Chybne zadany DKA."
			else do
				let filename = head filenames
				lines <- customFileParser filename
				let (allStatesList, startStateList, endStatesList, rules) = loadDKA $ words lines

				case loadAutomatData (allStatesList, startStateList, endStatesList, rules) of
					Just automat -> do 
						putStrLn $ id (printStates $ states automat)
						print $ initialState automat
						putStrLn $ id (printStates $ endStates automat)
						let transitionsStrings = map printTransitions $ delta automat
						mapM_ (\x -> putStrLn $ id x) transitionsStrings
						exitSuccess
					Nothing -> error "Chybne zadany DKA."

	-- "-t"
	when (not $ isNothing $ optShowMKA opts) $ do
		if null filenames
			then do
				allStates <- getLine
				startState <- getLine
				endStatesInput <- getLine
				rules <- getRules
				
				let allStatesList = wordsWhen (==',') allStates
				let startStateList = wordsWhen (==',') startState
				let endStatesList = wordsWhen (==',') endStatesInput

				case loadAutomatData (allStatesList, startStateList, endStatesList, rules) of
					Just automat -> do
						let updatedAutomat = (updateAutomat automat)
						let classes = updateMinimalisationClasses updatedAutomat $ initClasses updatedAutomat
						let minimalisationClasses = splitClasses updatedAutomat classes
						let minimalAutomat = getMKA (updatedAutomat, minimalisationClasses)
						putStrLn $ id (printStates $ states minimalAutomat)
						print $ initialState minimalAutomat
						putStrLn $ id (printStates $ endStates minimalAutomat)
						let transitionsStrings = map printTransitions $ delta minimalAutomat
						mapM_ (\x -> putStrLn $ id x) transitionsStrings
						exitSuccess
					Nothing -> error "Chybne zadany DKA."
				exitSuccess
			else do
				let filename = head filenames
				lines <- customFileParser filename
				let (allStatesList, startStateList, endStatesList, rules) = loadDKA $ words lines

				case loadAutomatData (allStatesList, startStateList, endStatesList, rules) of
					Just automat -> do
						let updatedAutomat = (updateAutomat automat)
						let classes = updateMinimalisationClasses updatedAutomat $ initClasses updatedAutomat
						let minimalisationClasses = splitClasses updatedAutomat classes
						let minimalAutomat = getMKA (updatedAutomat, minimalisationClasses)
						putStrLn $ id (printStates $ states minimalAutomat)
						print $ initialState minimalAutomat
						putStrLn $ id (printStates $ endStates minimalAutomat)
						let transitionsStrings = map printTransitions $ delta minimalAutomat
						mapM_ (\x -> putStrLn $ id x) transitionsStrings
						exitSuccess
					Nothing -> error "Chybne zadany DKA."
				exitSuccess

	error "Musis specifikovat aspon jeden parameter."
	exitFailure