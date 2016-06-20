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
	pensize t 2

	forM_ [0 .. 6] $ \x -> densha t 1 (100 + 70 * x) 200
	penup t
	writeChar t "black" 120 270 "でんしゃはいくつ?"
	setheading t 0
	goto t 250 350
	pendown t
	forward t 85

	forM_ [0 .. 4] $ \x -> densha t 1 (100 + 70 * x) 400
	penup t
	writeChar t "black" 120 470 "でんしゃはいくつ?"
	pensize t 2
	setheading t 0
	goto t 250 550
	pendown t
	forward t 85

	forM_ [0 .. 9] $ \x -> densha t 1 (100 + 70 * x) 600
	penup t
	writeChar t "black" 120 670 "でんしゃはいくつ?"
	pensize t 2
	setheading t 0
	goto t 250 750
	pendown t
	forward t 85

	svg <- getSVG t
	putStr $ showSVG 830 1200 svg
	waitField f
