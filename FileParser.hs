module FileParser where

import System.Directory

fileParser :: (String, Bool) -> IO String
fileParser (filename, True) = readFile filename
fileParser (_, False) = error "fail"

customFileParser :: String -> IO String
customFileParser filename = do
	fileExists <- doesFileExist filename
	if (fileExists) 
		then readFile filename
	else error "File does not exists."

