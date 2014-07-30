import Data.List
import Data.List.Split(wordsBy)
import qualified Data.Map as Map
import System.Environment(getArgs)

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
applyMove (Empty g) _ s = Map.adjust (const 0) g s
applyMove (Fill g) capacities s = Map.adjust (const $ capacities Map.! g) g s
applyMove (Pour from to) capacities s = 
    let
        toCapacity = capacities Map.! to
        fromState = s Map.! from
        toState = s Map.! to
        amount = min fromState (toCapacity - toState)
    in
        Map.adjust (const $ fromState - amount) from .
        Map.adjust (const $ toState + amount) to $ s

data Path = Path {
        endState :: State,
        history :: [Move]
    } deriving (Show)

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
            next <- map (extendPath path capacities) moves,
            (endState next) `notElem` explored]
        moreExplored = explored ++ (map endState morePaths)
    in
        [paths] ++ fromPaths morePaths moreExplored moves capacities

formatPath :: Path -> Capacities -> State -> [String]
formatPath path capacities initialState =
    let
        steps = foldr
            (\move acc ->
                let
                    previousState = snd $ head acc
                    nextState = applyMove move capacities previousState
                    stepDescription = show move ++ " => " ++ show nextState
                in
                    (stepDescription, nextState) : acc)
            [("initialState = " ++ show initialState, initialState)]
            (history path)
    in
        reverse $ map fst steps

solutions :: [[Path]] -> Volume -> [Path]
solutions pathSets target =
    [path |
        pathSet <- pathSets,
        path <- pathSet,
        target `elem` (Map.elems $ endState path)]

main = do

    args <- getArgs

    let
        glassVolumes = map read $ wordsBy (==',') $ args !! 0
        target = read $ args !! 1
        glasses = zipWith const [0..] glassVolumes
        capacities = Map.fromList $ zip glasses glassVolumes
        initialState = Map.fromList $ zip glasses $ repeat 0
        initialPath = Path initialState []
        moves =
            [Empty g | g <- glasses] ++
            [Fill g | g <- glasses] ++
            [Pour from to | from <- glasses, to <- glasses, from /= to]
        pathSets = fromPaths [initialPath] [initialState] moves capacities
        solution = head $ solutions pathSets target

    mapM_ putStrLn $ formatPath solution capacities initialState
