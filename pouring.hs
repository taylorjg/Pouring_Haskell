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
applyMove (Empty g) _ s = adjust (const 0) g s
applyMove (Fill g) capacities s = adjust (const $ capacities ! g) g s
applyMove (Pour from to) capacities s = 
    let
        toCapacity = capacities ! to
        fromState = s ! from
        toState = s ! to
        amount = min fromState (toCapacity - toState)
    in
        adjust (const $ fromState - amount) from .
        adjust (const $ toState + amount) to $ s

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
            next <- Data.List.map (extendPath path capacities) moves,
            (endState next) `notElem` explored]
        moreExplored = explored ++ (Data.List.map endState morePaths)
    in
        [paths] ++ fromPaths morePaths moreExplored moves capacities

formatPath :: Path -> Capacities -> State -> [String]
formatPath path capacities initialState =
    let
        steps = Data.List.foldr
            (\move acc ->
                let
                    previousState = case acc of
                        [] -> initialState
                        (_, ps):_ -> ps
                    nextState = applyMove move capacities previousState
                    stepDescription = show move ++ " => " ++ show nextState
                in
                    (stepDescription, nextState) : acc)
            []
            (history path)
    in
        reverse $ Data.List.map fst steps

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
        glasses = zipWith const [0..] glassVolumes
        capacities = fromList $ zip glasses glassVolumes
        initialState = fromList $ zip glasses $ repeat 0
        initialPath = Path initialState []
        moves =
            [Empty g | g <- glasses] ++
            [Fill g | g <- glasses] ++
            [Pour from to | from <- glasses, to <- glasses, from /= to]
        pathSets = fromPaths [initialPath] [initialState] moves capacities
        solution = head $ solutions pathSets target

    mapM_ putStrLn $ formatPath solution capacities initialState
