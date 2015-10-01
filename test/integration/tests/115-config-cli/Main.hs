import StackTest

main :: IO ()
main = do
    -- Test we are at baseline
    fileContentsMatch ["stack.yaml", "stack.yaml.base"]
    stack ["config", "set", "resolver", "nightly"]
    fileContentsMatch ["stack.yaml", "stack.yaml.nightly"]
