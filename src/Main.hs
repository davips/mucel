module Main(main) where
import View
import World

main :: IO ()
main = window update initialWorld
