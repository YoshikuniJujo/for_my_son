import Control.Monad
import Graphics.X11.Turtle

import Lib

main :: IO ()
main = run $ \t -> do
	pencolor t "blue"
	forM_ [0 .. 2] $ \y ->
		forM_ [0 .. 9] $ \x -> densha t 1 (50 + x * 80) (50 + y * 80)
	pencolor t "red"
	forM_ [0 .. 6] $ \x -> densha t 1 (50 + x * 80) (50 + 3 * 80)
	penup t
	(\xs ys f -> zipWithM_ f xs ys) [0 ..] [
		("blue", "あおいでんしゃはいくつ?"),
		("red", "あかいでんしゃはいくつ?"),
		("black", "でんしゃはぜんぶでいくつ?")] $ \y (clr, m) -> do
		writeChar t clr 50 (370 + 300 * y) m
		pencolor t clr
		goto t 400 (550 + 300 * y)
		setheading t 0
		pendown t
		forward t 150
		penup t
