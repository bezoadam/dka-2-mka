--DKA-2-MKA
--Adam Bezák xbezak01

import Control.Monad.State
import qualified Data.IntMap as M
import Data.Word
import System.Environment
import System.Exit
import System.Console.GetOpt
import Data.Maybe

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

main = do
	argv <- getArgs
	(opts, fname) <- compilerOpts argv
	print opts 
	print fname

	when (not $ isNothing $ optShowDKA opts) $ do
		if null fname
			then do
				print "SHOWDKA STDOUT"
				exitSuccess
			else do
				print "SHOWDKA FILE"
				exitSuccess
	when (not $ isNothing $ optShowMKA opts) $ do
		if null fname
			then do
				print "SHOWMKA STDOUT"
				exitSuccess
			else do
				print "SHOWDKA FILE"
				exitSuccess

	error "You need to specify one argument."
	exitFailure