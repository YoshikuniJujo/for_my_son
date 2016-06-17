import Control.Monad
import Graphics.X11.Turtle

import Lib

main :: IO ()
main = run $ \t -> do
	forM_ [0 .. 2] $ \y -> do
		forM_ [0 .. 9] $ \x ->
			hashigosha t 1 (20 + x * 80) (100 + y * 80)
		goto t 10 (78 + y * 80)
		setheading t 0
		pencolor t "gray"
		pendown t
		replicateM_ 2 $
			forward t 805 >> right t 90 >> forward t 70 >> right t 90
		penup t
	forM_ [0 .. 3] $ \x -> hashigosha t 1 (20 + x * 80) 340
	(\xs ys f -> zipWithM_ f xs ys) [0 ..] [
		"ひとつのはこのなかのはしごしゃはいくつ?",
		"はこはいくつ?",
		"はこにはいっているはしごしゃはぜんぶでいくつ?",
		"はこのそとのはしごしゃはいくつ?",
		"はしごしゃはぜんぶでいくつ?"] $ \y m -> do
		writeChar t "black" 40 (440 + y * 150) m
		setheading t 0
		goto t 700 (525 + y * 150)
		pendown t
		forward t 150
		penup t
