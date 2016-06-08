module Lib (
	run,
	put, writeChar, densha, densha1, densha2, kuruma, child,
	tomato) where

import Control.Monad
import Graphics.X11.Turtle
import Text.XML.YJSVG hiding (topleft)

run :: (Turtle -> IO a) -> IO a
run act = do
	f <- openField
	onkeypress f $ return . (/= 'q')
	topleft f
	t <- newTurtle f
	penup t
	pensize t 2
	goto t 100 100
	r <- act t
	svg <- getSVG t
	putStr $ showSVG 830 1200 svg
	waitField f
	return r

put :: ColorClass c => Turtle -> c -> Double -> Double -> IO ()
put t c x y = do
	goto t x y
	pencolor t c
	beginfill t
	circle t 30
	endfill t

writeChar :: ColorClass c => Turtle -> c -> Double -> Double -> String -> IO ()
writeChar t c x y s = do
	goto t x y
	pencolor t c
	write t "Gothic" 30 s

densha :: Turtle -> Double -> Double -> IO ()
densha t x y = do
	penup t
	setheading t 0
	goto t x y
	pendown t
	replicateM_ 2 $ do
		forward t 50
		right t 90
		forward t 30
		right t 90
	penup t
	goto t (x + 10) y
	pendown t
	setheading t 45
	replicateM_ 4 $ do
		forward t 10
		left t 90
	penup t
	goto t (x + 40) y
	pendown t
	setheading t 45
	replicateM_ 4 $ do
		forward t 10
		left t 90
	penup t
	goto t (x + 20) (y + 30)
	pendown t
	setheading t (- 90)
	replicateM_ 10 $ do
		forward t 1.5
		right t 20
	setheading t (- 90)
	replicateM_ 10 $ do
		forward t 1.5
		right t 20
	penup t
	goto t (x + 45) (y + 30)
	pendown t
	setheading t (- 90)
	replicateM_ 10 $ do
		forward t 1.5
		right t 20
	setheading t (- 90)
	replicateM_ 10 $ do
		forward t 1.5
		right t 20
	penup t
	setheading t 0
	goto t (x + 10) (y + 5)
	pendown t
	replicateM_ 4 $ forward t 10 >> right t 90
	penup t
	setheading t 0
	goto t (x + 30) (y + 5)
	pendown t
	replicateM_ 4 $ forward t 10 >> right t 90

kuruma :: ColorClass c => Turtle -> c -> Double -> Double -> IO ()
kuruma t c x y = do
	penup t
	pencolor t c
	setheading t 0
	goto t x y
	pendown t
	left t 60
	forward t 10
	right t 60
	forward t 10
	left t 60
	forward t 15
	right t 60
	forward t 20
	right t 60
	forward t 15
	left t 60
	forward t 10
	right t 60
	forward t 10
	right t 120
	forward t 15
	right t 90
	circle t 5
	penup t
	setheading t 180
	forward t 10
	pendown t
	forward t 20
	right t 90
	circle t 5
	penup t
	setheading t 180
	forward t 10
	pendown t
	forward t 10
	penup t
	goto t (x + 23) (y - 10)
	pendown t
	setheading t 0
	replicateM_ 4 $ forward t 6 >> left t 90
	penup t
	goto t (x + 33) (y - 10)
	pendown t
	setheading t 0
	replicateM_ 4 $ forward t 6 >> left t 90
	penup t

densha1 :: Turtle -> Double -> Double -> IO ()
densha1 t x y = do
	penup t
	setheading t 0
	goto t x y
	pencolor t "blue"
	beginfill t
	replicateM_ 2 $ do
		forward t 50
		right t 90
		forward t 30
		right t 90
	endfill t
	pencolor t "black"
	pendown t
	replicateM_ 2 $ do
		forward t 50
		right t 90
		forward t 30
		right t 90
	penup t

	do
		pencolor t "black"
		goto t (x + 10) y
		pendown t
		setheading t 45
		replicateM_ 4 $ do
			forward t 10
			left t 90
		penup t
		goto t (x + 40) y
		pendown t
		setheading t 45
		replicateM_ 4 $ do
			forward t 10
			left t 90
		penup t

	goto t (x + 20) (y + 30)
	pendown t
	setheading t (- 90)
	replicateM_ 10 $ do
		forward t 1.5
		right t 20
	setheading t (- 90)
	replicateM_ 10 $ do
		forward t 1.5
		right t 20
	penup t
	goto t (x + 45) (y + 30)
	pendown t
	setheading t (- 90)
	replicateM_ 10 $ do
		forward t 1.5
		right t 20
	setheading t (- 90)
	replicateM_ 10 $ do
		forward t 1.5
		right t 20
	penup t
	do
		pencolor t "yellow"
		setheading t 0
		goto t (x + 10) (y + 5)
		beginfill t
		replicateM_ 4 $ forward t 10 >> right t 90
		endfill t
		setheading t 0
		goto t (x + 30) (y + 5)
		beginfill t
		replicateM_ 4 $ forward t 10 >> right t 90
		endfill t
	do
		pencolor t "black"
		setheading t 0
		goto t (x + 10) (y + 5)
		pendown t
		replicateM_ 4 $ forward t 10 >> right t 90
		penup t
		setheading t 0
		goto t (x + 30) (y + 5)
		pendown t
		replicateM_ 4 $ forward t 10 >> right t 90

densha2 :: Turtle -> Double -> Double -> IO ()
densha2 t x y = do
	penup t
	setheading t 0
	goto t x y
	pencolor t "blue"
	beginfill t
	replicateM_ 2 $ do
		forward t 50
		right t 90
		forward t 30
		right t 90
	endfill t
	pencolor t "black"
	pendown t
	replicateM_ 2 $ do
		forward t 50
		right t 90
		forward t 30
		right t 90
	penup t

	do
		pencolor t "black"
		goto t (x + 10) y
		pendown t
		setheading t 45
		replicateM_ 4 $ do
			forward t 10
			left t 90
		penup t
		goto t (x + 40) y
		pendown t
		setheading t 45
		replicateM_ 4 $ do
			forward t 10
			left t 90
		penup t

	goto t (x + 20) (y + 30)
	pendown t
	setheading t (- 90)
	replicateM_ 10 $ do
		forward t 1.5
		right t 20
	setheading t (- 90)
	replicateM_ 10 $ do
		forward t 1.5
		right t 20
	penup t
	goto t (x + 45) (y + 30)
	pendown t
	setheading t (- 90)
	replicateM_ 10 $ do
		forward t 1.5
		right t 20
	setheading t (- 90)
	replicateM_ 10 $ do
		forward t 1.5
		right t 20
	penup t
	do
		pencolor t "black"
		setheading t 0
		goto t (x + 10) (y + 5)
		beginfill t
		replicateM_ 4 $ forward t 10 >> right t 90
		endfill t
		setheading t 0
		goto t (x + 30) (y + 5)
		beginfill t
		replicateM_ 4 $ forward t 10 >> right t 90
		endfill t
	do
		pencolor t "black"
		setheading t 0
		goto t (x + 10) (y + 5)
		pendown t
		replicateM_ 4 $ forward t 10 >> right t 90
		penup t
		setheading t 0
		goto t (x + 30) (y + 5)
		pendown t
		replicateM_ 4 $ forward t 10 >> right t 90

child :: ColorClass c => Turtle -> c -> Double -> Double -> IO ()
child t c x y = do
	penup t
	setheading t 0
	pencolor t c
	goto t x y
	forward t 10
	pendown t
	circle t 10
	left t 180
	circle t 10
	penup t
	goto t (x + 3) (y + 5)
	pendown t
	setheading t 135
	forward t 11
	penup t
	goto t (x + 17) (y + 5)
	pendown t
	setheading t 45
	forward t 11
	penup t
	goto t (x + 5) (y + 19)
	setheading t (- 90)
	pendown t
	forward t 13
	penup t
	goto t (x + 15) (y + 19)
	pendown t
	forward t 13
	penup t

tomato :: Turtle -> Double -> Double -> IO ()
tomato t x y = do
	goto t x y
	pencolor t "red"
	setheading t 180
	beginfill t
	circle t 30
	endfill t
	pencolor t "green"
	goto t x (y - 3)
	left t 10
	beginfill t
	forward t 10
	left t 60
	forward t 20
	left t 160
	forward t 20
	right t 130
	forward t 20
	left t 160
	forward t 20
	right t 130
	forward t 20
	left t 160
	forward t 20
	endfill t
	pencolor t "black"
	penup t
