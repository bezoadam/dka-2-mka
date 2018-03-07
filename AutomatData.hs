--DKA-2-MKA
--BC. Adam Bezák xbezak01

module AutomatData where

import Debug.Trace

-- Stav
type State = Int

-- Datovy typ reprezentujuci jeden rechod
data Transition = Transition 
	{
		from :: State,
		to :: State,
		value :: String
	} deriving (Eq, Ord, Show)

-- Definicia KA
data Automat = Automat 
	{
		states :: [State],
		sigma :: [String],		-- Abeceda
		delta :: [Transition],		-- Prechodova funkcia
		initialState :: State,		-- Startovaci stav
		endStates :: [State]		-- Mnozina koncovych stavov
	} deriving (Show, Eq, Ord)

-- Odstrani duplikaty z listu
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = rdHelper []
    where rdHelper seen [] = seen
          rdHelper seen (x:xs)
              | x `elem` seen = rdHelper seen xs
              | otherwise = rdHelper (seen ++ [x]) xs

-- Rozdeli String na zaklade delimetra
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

-- Pomocna funkcia na vytvorenie prechodu
makeTransition :: String -> Transition
makeTransition rule = do
	let words = wordsWhen (==',') rule
	Transition  { from = read (words !! 0) :: State, to = read (words !! 2) :: State, value = (words !! 1) }

-- Pomocna funkcia na vytvorenie abecedy
getAlphabetChar :: Transition -> String
getAlphabetChar transition = value transition

-- Vrati abecedu z prechodov
getSigma :: [Transition] -> [String]
getSigma transitions = trace ("somtututututu") removeDuplicates (map (getAlphabetChar) transitions)
