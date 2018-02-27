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

-------------------------------ARGUMENT PARSER----------------------------------

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

-------------------------------OUTPUT-------------------------------

printStates :: [String] -> String
printStates states = intercalate "," states

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

------------------------------MINIMALISATION----------------------------



-------------------------------MAIN---------------------------------

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
				allStates <- getLine
				startState <- getLine
				endStates <- getLine
				rules <- getRules
				
				let allStatesList = wordsWhen (==',') allStates
				let startStateList = wordsWhen (==',') startState
				let endStatesList = wordsWhen (==',') endStates

				case loadAutomatData (allStatesList, startStateList, endStatesList, rules) of
					Just automat -> do
						print automat
						exitSuccess
					Nothing -> error "Chybny DKA"
			else do
				let filename = head filenames
				lines <- customFileParser filename
				let (allStatesList, startStateList, endStatesList, rules) = loadDKA $ words lines

				case loadAutomatData (allStatesList, startStateList, endStatesList, rules) of
					Just automat -> do 
						print automat
						print $ minimizeAutomat automat
						let sigmaValue = "b"
						let initClass = initClasses automat !! 0
						let getCellTransitioForA = getCellTransition sigmaValue
						print $ getCellTransitioForA (classStates initClass, delta automat)

						exitSuccess
					Nothing -> error "Chybny DKA"

	when (not $ isNothing $ optShowMKA opts) $ do
		if null filenames
			then do
				print "SHOWMKA STDOUT"
				exitSuccess
			else do
				print "SHOWMKA FILE"
				exitSuccess

	error "You need to specify one argument."
	exitFailure