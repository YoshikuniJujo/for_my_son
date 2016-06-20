import Control.Monad
import Graphics.X11.Turtle

import Lib

main :: IO ()
main = run $ \t -> do
	forM_ [0 .. 5] $ \x -> hashigosha t 1 (20 + 80 * x) 50
	forM_ [6 .. 9] $ \x -> pomp t 4 (20 + 80 * x) 60
	forM_ [0 .. 3] $ \x -> pomp t 4 (20 + 80 * x) 130
	(\xs ys f -> zipWithM_ f xs ys) [0 ..] [
		"はしごしゃはいくつ?",
		"ぽんぷしゃはいくつ?",
		"しょうぼうしゃはぜんぶでいくつ?",
		"しきでかいてみよう" ] $ \y m -> do
		writeChar t "black" 20 (220 + 230 * y) m
		penup t
		setheading t 0
		goto t 500 (360 + 230 * y)
		pendown t
		forward t 150
		penup t
	goto t 50 (360 + 230 * 3)
	pendown t
	forward t 150
	penup t
	forward t 70
	pendown t
	forward t 150
	penup t
	writeChar t "black" 230 (320 + 230 * 3) "+"
	writeChar t "black" 450 (320 + 230 * 3) "="
