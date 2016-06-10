import Control.Monad
import Graphics.X11.Turtle

import Lib

main :: IO ()
main = run $ \t -> do
	forM_ [0 .. 3] $ \x -> dango t (100 + x * 50) 100
	forM_ [0 .. 3] $ \x -> dango_bara t (100 + x * 50) 600
	forM_ [
		(100, "だんごはなんぼん?"),
		(600, "くしからはずしたよ。だんごはいくつ?") ] $ \(y, m) -> do
		writeChar t "black" 100 (y + 150) m
		goto t 400 (y + 400)
		setheading t 0
		pendown t
		forward t 150
		penup t
