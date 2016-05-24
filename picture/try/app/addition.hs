module Main where

-- import Control.Monad
import Graphics.X11.Turtle
import Text.XML.YJSVG hiding (topleft)

main :: IO ()
main = do
	f <- openField
	onkeypress f $ return . (/= 'q')
	topleft f
	t <- newTurtle f
	penup t
	put t "red" 80 130
	put t "blue" 180 130
	put t "blue" 280 130
	put t "green" 380 130
	put t "green" 480 130
	put t "green" 580 130
	put t "green" 680 130
	writeChar t "red" 70 200 "1"
	writeChar t "black" 120 200 "+"
	writeChar t "blue" 220 200 "2"
	writeChar t "black" 320 200 "+"
	writeChar t "green" 520 200 "4"
	writeChar t "black" 750 200 "="
	writeChar t "black" 790 200 "7"
	svg <- getSVG t
	putStr $ showSVG 830 300 svg
	waitField f

put :: ColorClass c => Turtle -> c -> Double -> Double -> IO ()
put t c x y = do
	goto t x y
	pencolor t c
	beginfill t
	circle t 40
	endfill t

writeChar :: ColorClass c => Turtle -> c -> Double -> Double -> String -> IO ()
writeChar t c x y s = do
	goto t x y
	pencolor t c
	write t "Gothic" 50 s
