import Control.Monad

import Graphics.X11.Turtle
import Text.XML.YJSVG hiding (topleft)

import Lib

main :: IO ()
main = do
	f <- openField
	onkeypress f $ return . (/= 'q')
	topleft f
	t <- newTurtle f
	penup t
	forM_2 [0 .. 9] (replicate 8 "blue" ++ replicate 2 "red") $ \x c ->
		put t c (60 + 70 * x) 80
	forM_ [0 .. 4] $ \x -> put t "red" (60 + 70 * x) 150
	writeChar t "blue" 40 200 "あおいぼーるはいくつ?"
	writeChar t "red" 40 400 "あかいぼーるはいくつ?"
	writeChar t "black" 40 600 "ぼーるはぜんぶでいくつ?"
	writeChar t "black" 40 800 "+や=をつかってしきでかいてみよう"
	svg <- getSVG t
	putStr $ showSVG 830 1000 svg
	waitField f

forM_2 :: Monad m => [a] -> [b] -> (a -> b -> m c) -> m ()
forM_2 xs ys op = zipWithM_ op xs ys

colors :: [String]
colors = take 10 $ cycle ["red", "blue", "green", "orange"]
