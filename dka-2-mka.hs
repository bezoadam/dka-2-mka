--DKA-2-MKA
--Adam Bezák xbezak01

import Control.Monad.State
import qualified Data.IntMap as M
import Data.Word
import System.Environment
import System.Directory
import System.Exit
import System.Console.GetOpt
import System.IO
import Data.Maybe
import Data.Typeable
import Data.List

import FileParser
import Minimalisation
import DKAParser
import AutomatData

-------------------------------ARGUMENT PARSER----------------------

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
	    "Nacitanie a vypis vstupneho DKA na standartni vystup."
	, Option ['t'] []
	    (OptArg ((\ f opts -> opts { optShowMKA = Just f }) . fromMaybe "ShowMKA") "FILENAME")
	    "Nacitanie a transformacia DKA na MKA a vypis MKA na standartni vystup."
	]

compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
 	case getOpt Permute options argv of
     	(o,n,[]) -> return (foldl (flip id) defaultOptions o, n)
     	(_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
 	where header = "Pouziti: OPTION [FILENAME]"

-------------------------------OUTPUT-------------------------------

printStates :: Show a => [a] -> String
printStates = intercalate "," . map show

printTransitions :: Transition -> String
printTransitions transition = show (from transition) ++ "," ++ value transition ++ "," ++ show (to transition)

getMKA :: (Automat, [MinimalisationClass]) -> Automat
getMKA (automat, minimalisationClasses) = do 
						let minimalStates = getStatesFromMinimalisationClasses minimalisationClasses
						let minimalStartState = getStartStateFromMinimalisationClasses (initialState automat, minimalisationClasses)
						let minimalEndStates = getEndStatesFromMinimalisationClasses (endStates automat, minimalisationClasses)
						let minimalTransitions = concat $ getTransitionsFromMinimalisationClasses minimalisationClasses
						let transitionsStrings = map printTransitions minimalTransitions
						Automat { states = minimalStates, sigma = sigma automat, delta = minimalTransitions, initialState = minimalStartState, endStates = minimalEndStates }

-------------------------------INPUT--------------------------------

--Nacita vstup zo stdin
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
		error "Privela vstupnych suborov"

	when ((not $ isNothing $ optShowDKA opts) && (not $ isNothing $ optShowMKA opts)) $ do
		if null filenames
			then do
				print "SHOWDKA, SHOWMKA STDOUT"
				exitSuccess
			else do
				print "SHOWDKA, SHOWMKA FILE"
				exitSuccess

	-- "-t"
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
					Nothing -> error "Chybny DKA."
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
					Nothing -> error "Chybny DKA."

	-- "-i"
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
						let classes = updateMinimalisationClasses automat $ initClasses automat
						let minimalisationClasses = splitClasses automat classes
						let minimalAutomat = getMKA (automat, minimalisationClasses)
						putStrLn $ id (printStates $ states minimalAutomat)
						print $ initialState automat
						putStrLn $ id (printStates $ endStates automat)
						let transitionsStrings = map printTransitions $ delta minimalAutomat
						mapM_ (\x -> putStrLn $ id x) transitionsStrings
						exitSuccess
					Nothing -> error "Chybny DKA."
				exitSuccess
			else do
				let filename = head filenames
				lines <- customFileParser filename
				let (allStatesList, startStateList, endStatesList, rules) = loadDKA $ words lines

				case loadAutomatData (allStatesList, startStateList, endStatesList, rules) of
					Just automat -> do 
						let classes = updateMinimalisationClasses automat $ initClasses automat
						let minimalisationClasses = splitClasses automat classes
						let minimalAutomat = getMKA (automat, minimalisationClasses)
						putStrLn $ id (printStates $ states minimalAutomat)
						print $ initialState automat
						putStrLn $ id (printStates $ endStates automat)
						let transitionsStrings = map printTransitions $ delta minimalAutomat
						mapM_ (\x -> putStrLn $ id x) transitionsStrings
						exitSuccess
					Nothing -> error "Chybny DKA."
				exitSuccess

	error "Musi specifikovat aspon jeden parameter."
	exitFailure