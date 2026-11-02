-- No `module … where` header: an implicit Main, which is what a script
-- named after its executable rather than after a module looks like.
import Shared.Helper (greet)

main :: IO ()
main = putStrLn (greet "alpha")
