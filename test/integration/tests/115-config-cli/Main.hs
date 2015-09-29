import StackTest

main :: IO ()
main = do
    stack ["config", "set", "resolver", "nightly"]
    fileContentsMatch ["stack.yaml", "stack.yaml.base"]
