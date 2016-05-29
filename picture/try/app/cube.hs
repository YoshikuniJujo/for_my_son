import Control.Monad
import Graphics.X11.Turtle
import Text.XML.YJSVG hiding (topleft)

topMargin, leftMargin :: Double
topMargin = 150
leftMargin = 150

main :: IO ()
main = do
	f <- openField
	topleft f
	onkeypress f $ return . (/= 'q')
	t <- newTurtle f
	penup t
	goto t 50 50
	write t "KochiGothic" 30 "ふといせんできりとってはこをつくろう。"
	pensize t 1
	forM_ [(1, 0), (1, 1), (1, 2), (1, 3), (0, 1), (2, 1)] $ \(x, y) -> do
		goto t (leftMargin + 150 * x) (topMargin + 150 * y)
		square t
	pensize t 3
	forM_ [ (0, 1, 0), (- 90, 2, 0), (- 90, 2, 2), (- 90, 2, 3),
			(90, 1, 4), (90, 1, 3), (90, 1, 1) ] $ \(a, x, y) -> do
		setheading t a
		goto t (leftMargin + 150 * x) (topMargin + 150 * y)
		norisiro t
	forM_ [ (180, 1, 2), (0, 2, 1) ] $ \(a, x, y) -> do
		goto t (leftMargin + 150 * x) (topMargin + 150 * y)
		setheading t a
		pendown t 
		replicateM_ 3 $ forward t 150 >> right t 90
		penup t
	svg <- getSVG t
	putStr $ showSVG 830 1000 svg
	waitField f

square :: Turtle -> IO ()
square t = do
	pendown t
	replicateM_ 4 $ forward t 150 >> right t 90
	penup t

norisiro :: Turtle -> IO ()
norisiro t = do
	pendown t
	left t 60
	forward t 30
	right t 60
	forward t 120
	right t 60
	forward t 30
	penup t
