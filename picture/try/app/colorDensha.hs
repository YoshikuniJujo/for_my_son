import Control.Monad
import Graphics.X11.Turtle

import Lib

main :: IO ()
main = run $ \t -> do
	(\xs ys f -> zipWithM_ f xs ys) [0 ..] [
		(4, 12, "blue"),
		(2, 6, "red"),
		(5, 10, "yellow")] $ \y (dns, cld, clr) -> do
		pencolor t clr
		forM_ [0 .. dns - 1] $ \x ->
			densha t (100 + x * 80) (70 + y * 370)
		forM_ [0 .. cld - 1] $ \x ->
			child t clr (100 + x * 40) (140 + y * 370)
	pencolor t "black"
	(\xs ys f -> zipWithM_ f xs ys) [0 ..] [
		"ひとつのでんしゃにこどもたちはなんにんになる?",
		"ひとつのでんしゃにこどもたちはなんにんになる?",
		"ひとつのでんしゃにこどもたちはなんにんになる?" ] $ \y m -> do
		writeChar t "black" 100 (210 + y * 370) m
		goto t 550 (400 + y * 370)
		setheading t 0
		pendown t
		forward t 150
		penup t
