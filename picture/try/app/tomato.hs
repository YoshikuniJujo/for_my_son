import Control.Monad
import Graphics.X11.Turtle
import System.Environment

import Lib

main :: IO ()
main = run $ \t -> do
	[dad] <- getArgs
	pendown t
	forM_ [0 .. 5] $ \x -> tomato t (100 + 70 * x) 100
	penup t
	baby t 100 500
	mother t 200 500
	baby t 100 900
	mother t 200 900
	father t 300 900
	forM_ [
		(100, "とまとはいくつ?"),
		(500, "ままとふたりでわけるといくつずつ?"),
		(900, dad ++ "がかえってきたよ。いくつずつになる?") ] $ \(y, m) -> do
		penup t
		writeChar t "black" 100 (y + 100) m
		goto t 400 (y + 280)
		pendown t
		forward t 150

base :: Turtle -> Double -> Double -> IO ()
base t x y = do
	pencolor t "black"
	penup t
	goto t x y
	setheading t 180
	pendown t
	circle t 25
	penup t
	goto t (x + 25 * sqrt 2 / 2) (y + 25 - 25 * sqrt 2 / 2)
	setheading t 140
	beginfill t
	replicateM_ 9 $ forward t (25 * pi / 18) >> left t 10
	endfill t
	goto t (x - 10) (y + 32)
	setheading t 0
	pendown t
	forward t 20
	penup t

eyes :: Turtle -> Double -> Double -> IO ()
eyes t x y = do
	goto t (x - 10) (y + 15)
	setheading t 180
	beginfill t
	circle t 4
	endfill t
	goto t (x + 10) (y + 15)
	setheading t 180
	beginfill t
	circle t 4
	endfill t

glasses :: Turtle -> Double -> Double -> IO ()
glasses t x y = do
	goto t (x - 15) (y + 25)
	setheading t 0
	pendown t
	replicateM_ 4 $ forward t 10 >> left t 90
	penup t
	goto t (x + 5) (y + 25)
	setheading t 0
	pendown t
	replicateM_ 4 $ forward t 10 >> left t 90
	penup t

hair :: Turtle -> Double -> Double -> IO ()
hair t x y = do
	goto t (x - 25 * sqrt 2 / 2) (y + 25 + 25 * sqrt 2 / 2)
	setheading t 135
	beginfill t
	replicateM_ 9 $ forward t (25 * pi / 36) >> right t 5
	setheading t (- 90)
	forward t $ 25 * sqrt 2 / 2 + 3
	endfill t
	goto t (x + 25 * sqrt 2 / 2) (y + 25 + 25 * sqrt 2 / 2)
	setheading t 45
	beginfill t
	replicateM_ 9 $ forward t (25 * pi / 36) >> left t 5
	setheading t (- 90)
	forward t $ 25 * sqrt 2 / 2 + 3
	endfill t

baby, father, mother :: Turtle -> Double -> Double -> IO ()
baby t x y = do
	base t x y
	eyes t x y

father t x y = do
	base t x y
	glasses t x y

mother t x y = do
	base t x y
	eyes t x y
	hair t x y
