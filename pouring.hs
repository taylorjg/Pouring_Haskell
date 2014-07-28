import qualified Data.Map as Map

type Index = Int
type Volume = Int
type State = Map.Map Index Volume
type Capacities = Map.Map Index Volume

data Move =
	  Empty { glass :: Index }
	| Fill { glass :: Index }
	| Pour { from :: Index, to :: Index }

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
		Map.adjust (\_ -> toState + amount) to (Map.adjust (\_ -> fromState - amount) from s)

main = do

	let
		glassVolumes = [4, 9]
		capacities = Map.fromList $ zip [0..] glassVolumes
		initialState = Map.map (\_ -> 0) capacities
		move1 = Fill 1
		move2 = Pour 1 0
		move3 = Empty 0
		move4 = Pour 1 0
		newState1 = applyMove move1 capacities initialState
		newState2 = applyMove move2 capacities newState1
		newState3 = applyMove move3 capacities newState2
		newState4 = applyMove move4 capacities newState3

	putStrLn $ "capacities: " ++ show capacities
	putStrLn $ "initialState: " ++ show initialState
	putStrLn $ "newState: " ++ show newState4
