import Graphics.X11.Turtle
import Text.XML.YJSVG hiding (topleft)

main :: IO ()
main = do
	f <- openField
	onkeypress f $ return . (/= 'q')
	topleft f
	t <- newTurtle f
	penup t

	goto t 100 500
	pencolor t "gray"
	write t "MS Gothic" 200 "樹"

	svg <- getSVG t
	putStr $ showSVG 850 1200 svg
	waitField f
