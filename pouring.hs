type State = [Int]

data Move =
	Empty { glass :: Int } |
	Fill { glass :: Int } |
	Pour { from :: Int, to :: Int }

-- http://stackoverflow.com/questions/5852722/replace-individual-list-elements-in-haskell
replaceNth n newVal (x:xs)
	| n == 0 = newVal:xs
	| otherwise = x:replaceNth (n-1) newVal xs	

applyMove :: Move -> [Int] -> State -> State
applyMove m cs s = case m of
	Empty g ->
		replaceNth g 0 s
	Fill g ->
		replaceNth g (cs !! g) s
	Pour g1 g2 ->
		let
			toCapacity = cs !! g2
			fromState = s !! g1
			toState = s !! g2
			amount = min fromState (toCapacity - toState)
		in (replaceNth g2 (toState + amount) (replaceNth g1 (fromState - amount) s))

main = do

	let
		capacities = [4, 9]
		initialState = map (\_ -> 2) capacities :: State
		move = Pour 1 0
		newState = applyMove move capacities initialState

	putStrLn $ "capacities: " ++ show capacities
	putStrLn $ "initialState: " ++ show initialState
	putStrLn $ "newState: " ++ show newState
