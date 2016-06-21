import Control.Monad
import Graphics.X11.Turtle

import Lib

main :: IO ()
main = run $ \t -> do
	forM_ (zip [0 ..] [
		(kuruma t "blue", 5, "くるまはなんだい?"),
		(child t "black", 8, "こどもはなんにん?"),
		(tomato t, 3, "とまとはいくつ?"),
		(pomp t 4, 7, "ぽんぷしゃはなんだい?") ]) $ \(i, (p, n, m)) -> do
		forM_ [0 .. n] $ \j -> p (100 + 80 * j) (100 + 250 * i)
		writeChar t "black" 100 (190 + 250 * i) m
		goto t 500 (270 + 250 * i)
		pendown t
		setheading t 0
		forward t 150
		penup t
