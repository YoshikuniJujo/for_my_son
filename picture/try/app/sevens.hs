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
	forM_ [0 .. 1] $ \x -> put t "blue" (leftMargin + 70 * x) 100
	forM_ [2 .. 6] $ \x -> put t "red" (leftMargin + 70 * x) 100
	forM_ [0 .. 2] $ \x -> put t "blue" (leftMargin + 70 * x) 400
	forM_ [3 .. 6] $ \x -> put t "red" (leftMargin + 70 * x) 400

	forM_ [0 .. 3] $ \x -> put t "blue" (leftMargin + 70 * x) 700
	forM_ [4 .. 6] $ \x -> put t "red" (leftMargin + 70 * x) 700

	forM_ [0 .. 4] $ \x -> put t "blue" (leftMargin + 70 * x) 1000
	forM_ [5 .. 6] $ \x -> put t "red" (leftMargin + 70 * x) 1000
	forM_ [100, 400, 700, 1000] $ \y -> do
		writeChar t "blue" 40 (y + 50) "あおいぼーる"
		writeChar t "red" 240 (y + 50) "あかいぼーる"
		writeChar t "black" 440 (y + 50) "ぜんぶで?"
		writeChar t "black" 220 (y + 110) "+"
		writeChar t "black" 420 (y + 110) "="
		pensize t 2
		pencolor t "blue"
		setheading t 0
		goto t 40 (y + 150)
		pendown t
		forward t 160
		penup t
		forward t 50
		pencolor t "red"
		pendown t
		forward t 160
		penup t
		forward t 50
		pencolor t "black"
		pendown t
		forward t 160
		penup t


	svg <- getSVG t
	putStr $ showSVG 830 1300 svg
	waitField f
