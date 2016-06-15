import Control.Monad
import Graphics.X11.Turtle

import Lib

main :: IO ()
main = run $ \t -> do
	forM_ [0 .. 4] $ \y -> do
		forM_ [0 .. 9] $ \x -> pomp t 3 (50 + 70 * x) (30 + 60 * y)
		penup t
		goto t 40 (15 + 60 * y)
		pencolor t "gray"
		pendown t
		pensize t 2
		setheading t 0
		replicateM_ 2 $
			forward t 700 >> right t 90 >> forward t 50 >> right t 90
	forM_ [0 .. 5] $ \x -> pomp t 3 (50 + 70 * x) (30 + 60 * 5)
	pensize t 2
	pencolor t "black"
	penup t
	setheading t 0
	(\xs ys f -> zipWithM_ f xs ys) [0 ..] [
		"ひとつのはこのなかのぽんぷしゃはいくつ?",
		"はこのかっはいくつ?",
		"はこにはいっているぽんぷしゃはぜんぶでいくつ?",
		"はこにはいっていないぽんぷしゃはいくつ?",
		"ぽんぷしゃはぜんぶでいくつ?" ] $ \y m -> do
		writeChar t "black" 50 (410 + 150 * y) m
		goto t 650 (520 + 150 * y)
		pendown t
		forward t 150
		penup t
