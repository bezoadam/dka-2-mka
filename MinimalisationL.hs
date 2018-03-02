import AutomatData

data MinimalisationClass = MinimalisationClass {
    number :: Int,
    classStates :: [State],
    classTransitions :: Just [ClassTransition]
} deriving (Eq, Ord, Show)

data ClassTransition = ClassTransition {
    state :: State,
    symbol :: String,
    classNumber :: Int
} deriving (Eq, Ord, Show)

--Pre kazdu triedu zoberiem kazdy stav a pre kazdy znak z abecedy
-- vyskusakm do akej triedy padne. Ak nejaky padne do inej triedy nez ta prva tak ten stav
-- odstepim a dam do novej minimalizacnej triedy. + musim kontrolovat ci sa uz nenachadza
-- v minimalizacnej triede z predchadzajuceho stepenia ( z predchadzajuceho pismena)
-- Ukoncim ak v kazdej min. triede sa vsetky  znaky dostanu do tej istej triedy
minimizeAutomat :: Automat -> [MinimalisationClass]
minimizeAutomat automat = initClasses automat

initClasses :: Automat -> [MinimalisationClass]
initClasses automat = [
    MinimalisationClass { number = 1, classStates = endStates automat, classTransitions = Nothing },
    MinimalisationClass { number = 2, classStates = (states automat \\ endStates automat), classTransitions = Nothing }
    ]

— Vygeneruje všechny přechody pro všechny aktuální ekv. třídy
updateMinimalisationClass :: Automat -> [MinimalisationClass] -> [MinimalisationClass]
updateMinimalisationClass automat classes = map (\cls ->
    createClass (number cls) (classStates cls) (generateTransitionsAll (automat, classes) (classStates cls) (sigma automat))) classes

— Vytvoří ekv. Třídu pro zadané parametry
createClass :: Int -> [State] -> [ClassTransition] -> MinimalisationClass
createClass n cs ct = MinimalisationClass { number = n, classStates = cs, classTransitions = Just ct}

— Vygeneruje všechny 
generateTransitionsAll :: (Automat, [MinimalisationClass]) -> [State] -> [String] -> [ClassTransition]
generateTransitionsAll (automat, classes) states symbols = foldl (++) [] (map (\symbol -> generateTransitions (automat, classes) states symbol) symbols)

generateTransitions :: (Automat, [MinimalisationClass]) -> [State] -> String -> [ClassTransition]
generateTransitions (automat, classes) states symbol = map (\state -> generateTransition (automat, classes) state symbol) states

generateTransition :: (Automat, [MinimalisationClass]) -> State -> String -> ClassTransition
generateTransition (automat, classes) fromState pSymbol = do
        let toState = findToStateForTransition automat fromState pSymbol
        let number = findClassNumberForState classes toState
        ClassTransition {state = fromState, symbol = pSymbol, classNumber = number}


findClassNumberForState :: [MinimalisationClass] -> State -> Int
findClassNumberForState classes state =  number (head (filter (\x -> state `elem` (classStates x)) classes))

findToStateForTransition :: Automat -> State -> String -> State
findToStateForTransition automat fromState symbol = to (head (filter (\x -> (from x) == fromState && (value x) == symbol) transitions))
    where transitions = delta automat