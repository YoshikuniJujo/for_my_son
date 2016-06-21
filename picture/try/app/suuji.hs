import Control.Monad
import Graphics.X11.Turtle

import Lib

main :: IO ()
main = run $ \t -> do
	pencolor t (0xc0, 0xc0, 0xc0)
	forM_ [0 .. 9] $ \n -> do
		goto t 100 (100 + fromInteger n * 100)
		write t "Gothic" 80 $ show n
