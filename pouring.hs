import qualified Data.Map as Map

type Index = Int
type Volume = Int
type State = Map.Map Index Volume
type Capacities = Map.Map Index Volume

data Move
	= Empty { glass :: Index }
	| Fill { glass :: Index }
	| Pour { from :: Index, to :: Index }
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

extendPath :: Path -> Move -> Capacities -> Path
extendPath p m cs = Path (applyMove m cs (endState p)) (m : (history p))

fromPaths :: [Path] -> [State] -> [Move] -> Capacities -> [[Path]]
fromPaths [] _ _ _ = [[]]
fromPaths paths explored moves capacities =
	let
		morePaths = [next |
			path <- paths,
			next <- map (\m -> extendPath path m capacities) moves,
			(endState next) `notElem` explored]
		moreExplored = explored ++ (map (\p -> endState p) morePaths)
	in
		[paths] ++ fromPaths morePaths moreExplored moves capacities

main = do

	let
		glassVolumes = [4, 9]
		capacities = Map.fromList $ zip [0..] glassVolumes
		initialState = Map.map (\_ -> 0) capacities
		initialPath = Path initialState []
		pathSets = fromPaths [initialPath] [initialState] moves capacities

		glasses = [0..(length glassVolumes - 1)]
		moves =
			[Empty g | g <- glasses] ++
			[Fill g | g <- glasses] ++
			[Pour from to | from <- glasses, to <- glasses, from /= to]

	putStrLn $ "capacities: " ++ show capacities
	putStrLn $ "initialState: " ++ show initialState
	putStrLn $ "initialPath: " ++ show initialPath
	putStrLn $ "pathSets: " ++ show pathSets
