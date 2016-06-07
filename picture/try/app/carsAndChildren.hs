import Control.Monad
import Graphics.X11.Turtle
import Text.XML.YJSVG hiding (topleft)

import Lib

main :: IO ()
main = do
	f <- openField
	topleft f
	onkeypress f $ return . (/= 'q')
	t <- newTurtle f
	penup t
	pensize t 2

	forM_ [0 .. 6] $ \x -> kuruma t "black" (100 + 80 * x) 100

	forM_ [0 .. 2] $ \x -> child t "black" (100 + 80 * x) 400

	forM_ [
		(100, "おむかえのくるまはいくつ?"),
		(400, "こどもはなんにん?"),
		(700, "のこりのくるまはいくつ?") ] $ \(y, m) -> do
		writeChar t "black" 100 (y + 70) m
		penup t
		goto t 500 (y + 200)
		pendown t
		setheading t 0
		forward t 150
		penup t

	svg <- getSVG t
	putStr $ showSVG 830 1200 svg
	waitField f
