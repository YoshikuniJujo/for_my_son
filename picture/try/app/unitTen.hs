import Control.Monad

import Graphics.X11.Turtle
import Text.XML.YJSVG hiding (topleft)

import Lib

leftMargin, topMargin :: Double
leftMargin = 100
topMargin = 100

main :: IO ()
main = do
	f <- openField
	topleft f
	onkeypress f $ return . (/= 'q')
	t <- newTurtle f
	penup t

	forM_ [0 .. 9] $ \x -> put t "blue" (leftMargin + 70 * x) topMargin
	forM_ [0 .. 9] $ \x -> put t "blue" (leftMargin + 70 * x)
						(topMargin + 80)
	forM_ [0 .. 2] $ \x -> put t "blue" (leftMargin + 70 * x)
						(topMargin + 160)

	writeChar t "blue" 100 320 "あおいぼーるはいくつ?"
	pencolor t "black"
	pensize t 3
	setheading t 0
	goto t 500 420
	pendown t
	forward t 200
	penup t

	forM_ [0 .. 9] $ \x -> put t "blue" (leftMargin + 70 * x)
						(topMargin + 450)
	forM_ [0 .. 9] $ \x -> put t "blue" (leftMargin + 70 * x)
						(topMargin + 530)
	forM_ [0 .. 2] $ \x -> put t "blue" (leftMargin + 70 * x)
						(topMargin + 610)

	pencolor t "red"
	goto t (leftMargin - 35) (topMargin + 455)
	pendown t
	replicateM_ 2 $ do
		forward t 700
		left t 90
		forward t 70
		left t 90
	penup t
	goto t (leftMargin - 35) (topMargin + 535)
	pendown t
	replicateM_ 2 $ do
		forward t 700
		left t 90
		forward t 70
		left t 90
	penup t

	writeChar t "red" 100 770 "1つのあかいはこのなかに"
	writeChar t "blue" 450 770 "あおいぼーるはいくつ?"
	pencolor t "black"
	pensize t 3
	setheading t 0
	goto t 500 870
	pendown t
	forward t 200
	penup t

	writeChar t "red" 100 920 "あかいはこはいくつ?"
	pencolor t "black"
	pensize t 3
	setheading t 0
	goto t 500 1020
	pendown t
	forward t 200
	penup t

	writeChar t "blue" 100 1070 "あかいはこにはいっていないあおいぼーるはいくつ?"
	pencolor t "black"
	pensize t 3
	setheading t 0
	goto t 500 1170
	pendown t
	forward t 200
	penup t

	svg <- getSVG t
	putStr $ showSVG 830 1200 svg
	waitField f
