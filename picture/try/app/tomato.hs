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
