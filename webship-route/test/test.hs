import           Control.Monad (unless)
import           System.Exit (exitFailure)
import           System.IO (BufferMode (..), stdout, stderr, hSetBuffering)

import qualified Test.Webship.Route


main :: IO ()
main =
  testAll [
      Test.Webship.Route.tests
    ]


testAll :: [IO Bool] -> IO ()
testAll tests = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  sequence tests >>= \rs -> unless (and rs) exitFailure
