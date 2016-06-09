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

dango, dango_bara :: Turtle -> Double -> Double -> IO ()
dango t x_ y = do
	penup t
	goto t x y
	setheading t 180
	pendown t
	circle t 10
	penup t
	goto t x (y + 20)
	pendown t
	circle t 10
	penup t
	goto t x (y + 40)
	pendown t
	circle t 10
	penup t
	goto t x y
	setheading t 90
	pendown t
	forward t 7
	penup t
	goto t x (y + 60)
	setheading t (- 90)
	pendown t
	forward t 12
	penup t
	where
	x = x_ + 10

dango_bara t x_ y = do
	penup t
	goto t x y
	setheading t 180
	pendown t
	circle t 10
	penup t
	goto t x (y + 25)
	pendown t
	circle t 10
	penup t
	goto t x (y + 50)
	pendown t
	circle t 10
	penup t
	where
	x = x_ + 10
