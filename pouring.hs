import qualified Data.Map as Map

type Glass = Int
type Volume = Int
type Capacities = Map.Map Glass Volume
type State = Map.Map Glass Volume

data Move
	= Empty { glass :: Glass }
	| Fill { glass :: Glass }
	| Pour { from :: Glass, to :: Glass }
	deriving (Show)

applyMove :: Move -> Capacities -> State -> State
applyMove (Empty g) cs s = Map.adjust (\_ -> 0) g s
applyMove (Fill g) cs s = Map.adjust (\_ -> cs Map.! g) g s
applyMove (Pour from to) cs s = 
	let
		toCapacity = cs Map.! to
		fromState = s Map.! from
		toState = s Map.! to
		amount = min fromState (toCapacity - toState)
	in
		Map.adjust (\_ -> fromState - amount) from .
		Map.adjust (\_ -> toState + amount) to $ s

data Path = Path {
	endState :: State,
	history :: [Move]}
	deriving (Show)

extendPath :: Path -> Capacities -> Move -> Path
extendPath path capacities move =
	let
		newPath = applyMove move capacities (endState path)
		newHistory = move : history path
	in
		Path newPath newHistory

fromPaths :: [Path] -> [State] -> [Move] -> Capacities -> [[Path]]
fromPaths [] _ _ _ = [[]]
fromPaths paths explored moves capacities =
	let
		morePaths = [next |
			path <- paths,
			next <- map (extendPath path capacities) moves,
			(endState next) `notElem` explored]
		moreExplored = explored ++ (map endState morePaths)
	in
		[paths] ++ fromPaths morePaths moreExplored moves capacities

solutions :: [[Path]] -> Volume -> [Path]
solutions pathSets target =
	[path |
		pathSet <- pathSets,
		path <- pathSet,
		target `elem` (Map.elems $ endState path)]

main = do

	let
		glassVolumes = [4, 9]
		capacities = Map.fromList $ zip [0..] glassVolumes
		initialState = Map.map (\_ -> 0) capacities
		initialPath = Path initialState []
		pathSets = fromPaths [initialPath] [initialState] moves capacities
		answer = head $ solutions pathSets 7
		glasses = [0..(length glassVolumes - 1)]
		moves =
			[Empty g | g <- glasses] ++
			[Fill g | g <- glasses] ++
			[Pour from to | from <- glasses, to <- glasses, from /= to]

	putStrLn $ "capacities: " ++ show capacities
	putStrLn $ "initialState: " ++ show initialState
	putStrLn $ "initialPath: " ++ show initialPath
	putStrLn $ "answer: " ++ show answer
