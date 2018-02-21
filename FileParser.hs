module FileParser where

import System.Directory

fileParser :: (String, Bool) -> IO String
fileParser (filename, True) = readFile filename
fileParser (_, False) = error "fail"