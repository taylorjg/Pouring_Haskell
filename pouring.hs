import Data.Map
import Data.List
import Data.List.Split
import System.Environment

type Glass = Int
type Volume = Int
type Capacities = Map Glass Volume
type State = Map Glass Volume

data Move
	= Empty { glass :: Glass }
	| Fill { glass :: Glass }
	| Pour { from :: Glass, to :: Glass }
	deriving (Show)

applyMove :: Move -> Capacities -> State -> State
applyMove (Empty g) _ s = adjust (\_ -> 0) g s
applyMove (Fill g) capacities s = adjust (\_ -> capacities ! g) g s
applyMove (Pour from to) capacities s = 
	let
		toCapacity = capacities ! to
		fromState = s ! from
		toState = s ! to
		amount = min fromState (toCapacity - toState)
	in
		adjust (\_ -> fromState - amount) from .
		adjust (\_ -> toState + amount) to $ s

data Path = Path {
	endState :: State,
	history :: [Move]}
	deriving (Show)

extendPath :: Path -> Capacities -> Move -> Path
extendPath path capacities move =
	let
		newPath = applyMove move capacities $ endState path
		newHistory = move : history path
	in
		Path newPath newHistory

fromPaths :: [Path] -> [State] -> [Move] -> Capacities -> [[Path]]
fromPaths [] _ _ _ = [[]]
fromPaths paths explored moves capacities =
	let
		morePaths = [next |
			path <- paths,
			next <- Data.List.map (extendPath path capacities) moves,
			(endState next) `notElem` explored]
		moreExplored = explored ++ (Data.List.map endState morePaths)
	in
		[paths] ++ fromPaths morePaths moreExplored moves capacities

solutions :: [[Path]] -> Volume -> [Path]
solutions pathSets target =
	[path |
		pathSet <- pathSets,
		path <- pathSet,
		target `elem` (elems $ endState path)]

main = do

	args <- getArgs

	let
		glassVolumes = Data.List.map read $ wordsBy (==',') $ args !! 0
		target = read $ args !! 1
		glasses = [0..(length glassVolumes - 1)]
		capacities = fromList $ zip [0..] glassVolumes
		initialState = Data.Map.map (\_ -> 0) capacities
		initialPath = Path initialState []
		pathSets = fromPaths [initialPath] [initialState] moves capacities
		solution = head $ solutions pathSets target
		moves =
			[Empty g | g <- glasses] ++
			[Fill g | g <- glasses] ++
			[Pour from to | from <- glasses, to <- glasses, from /= to]

	putStrLn $ "capacities: " ++ show capacities
	putStrLn $ "initialState: " ++ show initialState
	putStrLn $ "initialPath: " ++ show initialPath
	putStrLn $ "solution: " ++ show solution
