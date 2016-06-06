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

	forM_ [0 .. 3] $ \x -> densha1 t (100 + 70 * x) 200
	forM_ [4 .. 6] $ \x -> densha2 t (100 + 70 * x) 200
	penup t

	writeChar t "black" 120 270 "でんしゃはぜんぶでいくつ?"
	setheading t 0
	goto t 450 390
	pendown t
	forward t 85
	penup t

	writeChar t "black" 120 440 "でんきがきえてるでんしゃはいくつ?"
	setheading t 0
	goto t 450 580
	pendown t
	forward t 85
	penup t

	writeChar t "black" 120 650
		"でんきがきえてるでんしゃはしゃこにかえりました。"
	writeChar t "black" 120 680 "のこりはいくつ?"
	setheading t 0
	goto t 450 770
	pendown t
	forward t 85
	penup t

	svg <- getSVG t
	putStr $ showSVG 830 1200 svg
	waitField f
