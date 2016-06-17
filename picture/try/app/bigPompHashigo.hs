import Lib

main :: IO ()
main = run $ \t -> do
	hashigosha t 8 100 250
	pomp t 32 100 750
