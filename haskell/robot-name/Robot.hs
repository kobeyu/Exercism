module Robot (robotName, mkRobot, resetName) where
import System.Random(randomRIO)
import Data.IORef(IORef,newIORef,readIORef,writeIORef)

type Robot = IORef String

robotName :: Robot -> IO String
robotName = readIORef 

mkName :: IO String
mkName = mapM randomRIO [c,c,d,d,d]
	where
		c = ('A','Z')
		d = ('0','9')

mkRobot :: IO Robot
mkRobot = mkName >>= newIORef

resetName :: Robot -> IO ()
resetName = (mkName >>=) .  writeIORef 
