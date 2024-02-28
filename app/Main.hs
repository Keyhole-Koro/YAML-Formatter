import System.IO

import YAMLTokenizer
import YAMLValidateSyntax

main :: IO ()
main = do
    let filePath = "sample/yamlSample1.txt"
    handle <- openFile filePath ReadMode
    tokens <- tokenize handle
    print tokens
    validateYAMLSyntax tokens
    hClose handle