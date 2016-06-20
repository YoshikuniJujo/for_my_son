import Lib
import Graphics.X11.Turtle

main :: IO ()
main = run $ \t -> do
	pencolor t "blue"
	densha t 7 100 120
	pencolor t "red"
	densha t 7 100 520
	pencolor t "yellow"
	densha t 7 100 920
