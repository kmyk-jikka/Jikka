main :: IO ()
main = putStrLn "Please rebuild with compile-web-assembly flag in the Asterius container. e.g. $ docker run -it --rm -v $(pwd):/workspace -w /workspace terrorjack/asterius cabal new-build --flag compile-web-assembly"
