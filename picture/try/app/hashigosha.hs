import Control.Monad
import Graphics.X11.Turtle

import Lib

main :: IO ()
main = run $ \t -> do
	forM_ [0 .. 2] $ \y -> do
		forM_ [0 .. 9] $ \x -> hashigosha t (20 + x * 80) (100 + y * 80)
		goto t 10 (78 + y * 80)
		setheading t 0
		pencolor t "gray"
		pendown t
		replicateM_ 2 $
			forward t 805 >> right t 90 >> forward t 70 >> right t 90
		penup t
	forM_ [0 .. 3] $ \x -> hashigosha t (20 + x * 80) 340
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

hashigosha :: Turtle -> Double -> Double -> IO ()
hashigosha t x y = do
	penup t
	setheading t 0
	goto t x y
	pencolor t "red"
	beginfill t
--	pendown t
	replicateM_ 2 $ forward t 40 >> right t 90 >> forward t 35 >> right t 90
	endfill t
--	penup t
	goto t (x + 45) y
	beginfill t
--	pendown t
--	forward t 25 >> right t 90 >> forward t 35 >> right t 90
	forward t 10 >> right t 60 >> forward t 20 >> right t 30 >>
		forward t (35 - 10 * sqrt 3) >> right t 90
	forward t 20 >> right t 90 >> forward t 35 >> right t 90
	endfill t
--	penup t
	forM_ [17, 46] $ \dx -> do
		goto t (x + dx) (y + 45)
		setheading t 0
		pencolor t "black"
		beginfill t
		circle t 10
		endfill t
	forM_ [0 .. 8] $ \d -> do
		goto t (x + 4 + 7.5 * d) (y + 4 - 2 * d)
		setheading t 135
		pendown t
		forward t 9
		penup t
	forM_ [0 .. 1] $ \d -> do
		goto t (x + 4 - d * 9 * sqrt 2 / 2) (y + 4 - d * 9 * sqrt 2 / 2)
		pendown t
		goto t	(x + 4 + 7.5 * 8 - d * 9 * sqrt 2 / 2)
			(y + 4 - 2 * 8 - d * 9 * sqrt 2 / 2)
		penup t
