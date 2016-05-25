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
	forM_2 [0 .. ] colors $ \y c -> do
		writeChar t "black" 20 (110 + 70 * y) (show $ floor y + 1)
		writeChar t "black" 760 (110 + 70 * y) (show $ (floor y + 1) * 10)
		forM_ [0 .. 9] $ \x -> put t c (90 + 70 * x) (130 + 70 * y)
	svg <- getSVG t
	putStr $ showSVG 830 1000 svg
	waitField f

forM_2 :: Monad m => [a] -> [b] -> (a -> b -> m c) -> m ()
forM_2 xs ys op = zipWithM_ op xs ys

colors :: [String]
colors = take 10 $ cycle ["red", "blue", "green", "orange"]
