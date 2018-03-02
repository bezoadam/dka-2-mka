--DKA-2-MKA
--Adam Bezák xbezak01

module FileParser where

import System.Directory

customFileParser :: String -> IO String
customFileParser filename = do
	fileExists <- doesFileExist filename
	if (fileExists) 
		then readFile filename
	else error "File does not exists."

