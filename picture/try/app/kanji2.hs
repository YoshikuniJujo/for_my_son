import Control.Monad
import Graphics.X11.Turtle
import Lib

main :: IO ()
main = run $ \t -> do
	forM_ (zip [0 ..] [ "机", "折り紙", "時計"]) $ \(y, k) -> do
		goto t 100 (200 + 250 * y)
		pencolor t (0xd0, 0xd0, 0xd0)
		write t "MS Gothic" 200 k
