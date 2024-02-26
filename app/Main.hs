import System.IO

import YAMLTokenizer

main :: IO ()
main = do
    let filePath = "sample/yamlSample1.txt"
    handle <- openFile filePath ReadMode
    content <- hGetChar handle
    tokens <- tokenize handle
    print tokens
    
    hClose handle