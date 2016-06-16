module Lib (
	run,
	put, writeChar, densha, densha1, densha2, kuruma, child,
	tomato, baby, mother, father, dango_bara, dango, pomp, hashigosha) where

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

base :: Turtle -> Double -> Double -> IO ()
base t x y = do
	pencolor t "black"
	penup t
	goto t x y
	setheading t 180
	pendown t
	circle t 25
	penup t
	goto t (x + 25 * sqrt 2 / 2) (y + 25 - 25 * sqrt 2 / 2)
	setheading t 140
	beginfill t
	replicateM_ 9 $ forward t (25 * pi / 18) >> left t 10
	endfill t
	goto t (x - 10) (y + 32)
	setheading t 0
	pendown t
	forward t 20
	penup t

eyes :: Turtle -> Double -> Double -> IO ()
eyes t x y = do
	goto t (x - 10) (y + 15)
	setheading t 180
	beginfill t
	circle t 4
	endfill t
	goto t (x + 10) (y + 15)
	setheading t 180
	beginfill t
	circle t 4
	endfill t

glasses :: Turtle -> Double -> Double -> IO ()
glasses t x y = do
	goto t (x - 15) (y + 25)
	setheading t 0
	pendown t
	replicateM_ 4 $ forward t 10 >> left t 90
	penup t
	goto t (x + 5) (y + 25)
	setheading t 0
	pendown t
	replicateM_ 4 $ forward t 10 >> left t 90
	penup t

hair :: Turtle -> Double -> Double -> IO ()
hair t x y = do
	goto t (x - 25 * sqrt 2 / 2) (y + 25 + 25 * sqrt 2 / 2)
	setheading t 135
	beginfill t
	replicateM_ 9 $ forward t (25 * pi / 36) >> right t 5
	setheading t (- 90)
	forward t $ 25 * sqrt 2 / 2 + 3
	endfill t
	goto t (x + 25 * sqrt 2 / 2) (y + 25 + 25 * sqrt 2 / 2)
	setheading t 45
	beginfill t
	replicateM_ 9 $ forward t (25 * pi / 36) >> left t 5
	setheading t (- 90)
	forward t $ 25 * sqrt 2 / 2 + 3
	endfill t

baby, father, mother :: Turtle -> Double -> Double -> IO ()
baby t x y = do
	base t x y
	eyes t x y

father t x y = do
	base t x y
	glasses t x y

mother t x y = do
	base t x y
	eyes t x y
	hair t x y

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

pomp :: Turtle -> Double -> Double -> Double -> IO ()
pomp t s x y = do
	setheading t 0
	penup t
	goto t x y
	pencolor t "red"
	beginfill t
	replicateM_ 2 $
		forward t (10 * s) >> right t 90 >> forward t (7 * s) >> right t 90
	endfill t
	goto t (x + 11 * s) (y - 1 * s)
	beginfill t
	forward t (3 * s) >> right t 60 >> forward t (6 * s) >> right t 30
	forward t ((8 - 3 * sqrt 3) * s) >> right t 90 >> forward t (6 * s)
	endfill t
	forM_ [3, 14] $ \dx -> do
		goto t (x + dx * s) (y + 9 * s)
		pencolor t "black"
		setheading t 0
		beginfill t
		circle t (1.5 * s)
		endfill t
	goto t (x + 5 * s) (y + 5.5 * s)
	setheading t 180
	pencolor t "white"
	pensize t (5 * s / 10)
	uzumaki t (0.12 * s)
	setheading t 0
	pensize t (3 * s / 10)
	forM_ [7.5, 9] $ \dx -> do
		goto t (x + dx * s) (y + 6 * s)
		pendown t
		circle t (4 * s / 10)
		penup t

uzumaki :: Turtle -> Double -> IO ()
uzumaki t s = do
	pendown t
	forM_ [0, 0.1 * s .. 3.6 * 2 * s] $ \x -> do
		forward t (3.6 * s - 0.4 * x)
		right t 10
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
