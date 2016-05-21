module Main where

import Control.Monad
import Graphics.X11.Turtle
import Text.XML.YJSVG hiding (topleft)

main :: IO ()
main = do
	f <- openField
	topleft f
	t <- newTurtle f
	penup t
	goto t 100 300
	pencolor t "blue"
	beginfill t
	replicateM_ 2 $ do
		forward t 40
		left t 90
		forward t 150
		left t 90
	endfill t
	beginfill t
	backward t 30
	left t 60
	forward t 60
	endfill t
	goto t 140 300
	setheading t 0
	beginfill t
	forward t 30
	left t 120
	forward t 60
	endfill t
	goto t 100 150
	setheading t 0
	pencolor t "red"
	beginfill t
	forward t 40
	left t 120
	forward t 40
	endfill t
	svg <- getSVG t
	putStr $ showSVG 300 300 svg
	onkeypress f $ return . (/= 'q')
	waitField f
