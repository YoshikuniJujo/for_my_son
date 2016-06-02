module Lib (put, writeChar, densha) where

import Control.Monad
import Graphics.X11.Turtle

someFunc :: IO ()
someFunc = putStrLn "someFunc"

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
