--DKA-2-MKA
--Bc. Adam Bezák xbezak01

module FileParser where

import System.Directory

-- Zisti ci zadany subor existuje a ak ano vrati jeho obsah
customFileParser :: String -> IO String
customFileParser filename = do
	fileExists <- doesFileExist filename
	if (fileExists) 
		then readFile filename
	else error "File does not exists."

