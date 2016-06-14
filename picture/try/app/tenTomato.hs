import Control.Monad
import Graphics.X11.Turtle

import Lib

main :: IO ()
main = run $ \t -> do
	forM_ [0 .. 3] $ \y -> do
		forM_ [0 .. 9] $ \x -> tomato t (80 + 70 * x) (20 + 85 * y)
		pencolor t "gray"
		goto t 40 (10 + 85 * y)
		pendown t
		setheading t 0
		replicateM_ 2 $
			forward t 710 >> right t 90 >> forward t 75 >> right t 90
		penup t
	forM_ [0 .. 4] $ \x -> tomato t (80 + 70 * x) (20 + 85 * 4)
	(\xs ys f -> zipWithM_ f xs ys) [0 ..] [
		"ひとつのはこのなかのとまとはいくつ?",
		"はこはいくつ?",
		"はこのなかのとまとはいくつ?",
		"はこにはいっていないとまとはいくつ?" ] $ \y m -> do
		writeChar t "black" 50 (460 + 195 * y) m
		goto t 500 (570 + 195 * y)
		setheading t 0
		pencolor t "black"
		pendown t
		forward t 150
		penup t
