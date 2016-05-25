module Lib (put, writeChar) where

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
