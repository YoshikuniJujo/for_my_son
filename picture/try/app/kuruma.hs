import Control.Monad
import Graphics.X11.Turtle
import Text.XML.YJSVG hiding (topleft)

import Lib

main :: IO ()
main = do
	f <- openField
	onkeypress f $ return . (/= 'q')
	topleft f
	t <- newTurtle f

	pensize t 2
	forM_ [0 .. 3] $ \x -> kuruma t "blue" (50 + 80 * x) 100
	forM_ [4 .. 6] $ \x -> kuruma t "red" (50 + 80 * x) 100
	forM_ [7 .. 9] $ \x -> kuruma t "green" (50 + 80 * x) 100
	forM_ [0 .. 1] $ \x -> kuruma t "green" (50 + 80 * x) 140
	forM_ [
		("blue", 190, "あおいくるまは"),
		("red", 350, "あかいくるまは"),
		("green", 510, "みどりのくるまは"),
		("black", 670, "くるまはぜんぶで") ] $ \(c, y, m) -> do
		writeChar t c 50 y $ m ++ "いくつ?"
		penup t
		goto t 400 (y + 110)
		pendown t
		setheading t 0
		forward t 150
		penup t

	svg <- getSVG t
	putStr $ showSVG 850 1200 svg
	waitField f
